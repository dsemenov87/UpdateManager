namespace MAptekaGet

[<RequireQualifiedAccess>]
module App =
  open System
  open System.Collections.Generic
  open System.Net
  open System.Threading.Tasks

  open Suave
  open Suave.Http
  open Suave.Headers
  open Suave.Operators
  open Suave.Filters
  open Suave.Successful
  open Suave.RequestErrors
  
  open DataAccess
  
  module NEL  = NonEmptyList
  module DR   = DependencyResolution
  module Rep  = Reporting
  module UP   = UpdaterProgram

  type Config =
    { IP            : Net.IPAddress
      Port          : Sockets.Port
      EscConvertUri : Uri
      EscUriPrefix  : UpdateName -> Domain.Version -> Uri
    }
  
  type Services =
    { Db            : IDbContext
      EscRepository : EscRepository
    }

  type private HttpInterpreterState =
    { Message: DomainMessage
    }

  /// classify domain message as WebPart
  let classifyDomMessage dmsg =
    match dmsg with
    | AvailableMessage (ListAvailable _) ->
        OK (string dmsg) >=> Writers.setMimeType "application/json"
    
    | Never
    | ValidationMessage (Ok _)
    | VersionCheckMessage (CorrectVersion _)
    | ResolutionMessage (Solution _) 
    | PublishMessage (Published _) ->
        OK (string dmsg) >=> Writers.setMimeType "text/plain"

    | VersionCheckMessage _
    | ResolutionMessage _
    | ValidationMessage _
    | PublishMessage _
    | AvailableMessage _ ->
        BAD_REQUEST (string dmsg)

    | UnexpectedError err -> // Internal Server Error
        Response.response HTTP_500 (err |> Text.Encoding.UTF8.GetBytes)
        
  [<AutoOpen>]
  module private Helpers =
    open Parsing
    open Utils.Parsing
    open ResultOp
    open Chiron

    let inline keepGoingIfSucceed (state: HttpInterpreterState) next f = function
      | Ok (x, dmsg) ->
          f ({state with Message = dmsg}) (next x)
      | Error dmsg ->
          classifyDomMessage dmsg

    let rec addConstraintRes<'err> (upd: Result<Update, 'err>) (constrs: Constraint list) =
      match constrs with
      | []          -> upd
      | constr::cs  -> addConstraintRes (upd <!> addConstraint constr) cs
    
    let private validateUpdate name vers =
      update <-- (name + "-" + vers)

    let validateUpdateAndConstrs name vers constrs =  
      constrs
      |> List.map ((<--) dependency)
      |> Result.sequence
      >>= addConstraintRes (validateUpdate name vers)
      <!> (fun upd -> (upd, upd |> Ok |> ValidationMessage))
      <?> Error
      <?> ValidationMessage

    let checkVersion (db: IDbContext) (upd: Update) =
      match db.GetVersionsByName upd.Name with
      | Ok upds ->
          let dmsg = VersionCheckMessage 
          match upds
              |> Seq.map (fun u -> u.Version) 
              |> Seq.filter ((>=) upd.Version)
              |> Seq.sortDescending
              |> Seq.tryHead with
          | Some v when upd.Version > v ->
              Error (dmsg (UnexpectedVersion (upd, v)))
          
          | Some v ->
              Error (dmsg (AlreadyPublished upd))
          
          | None ->
              Ok (upd, dmsg (CorrectVersion upd))

      | Error err ->
          failwith err // todo handle accurately

    let resolveDependencies (db: IDbContext) (updates: Update Set) =
      let dmsg = ResolutionMessage
      let res =
        match Set.toList updates with
        | [] -> Solution [] |> Ok
        | u::us ->
            updates
            |> db.GetDependencies
            <!> DR.resolve
            <*> Ok (NEL.create u us)
      
      match res with
      | Ok (Solution tree as sol) ->
          Ok (tree, dmsg sol)
      | Ok res ->
          Error (dmsg res)                                   
      | Error err ->
          failwith err

    let private setMessage msg state =
      {state with Message = msg}

    let uploadEsc (cfg: Config) customerId (upd, uri: Uri) =
      async {
        use downloader = new Http.HttpClient()
        
        use formContent =
          new Http.FormUrlEncodedContent
            ([  KeyValuePair("MailParticipantID", customerId)
                KeyValuePair("StorageUrl", uri.Host)
                KeyValuePair("DirResult", "esc")
                KeyValuePair("PathUPD", uri.PathAndQuery)
            ])
        
        let! response =
          downloader.PostAsync(cfg.EscConvertUri, formContent) |> Async.AwaitTask
        let! responseStream =
          response.Content.ReadAsStreamAsync() |> Async.AwaitTask

        use uploader = new Http.HttpClient()
        use reader = new IO.BinaryReader(responseStream)

        let status = reader.ReadChars 7 |> System.String |> String.toLowerInvariant
        
        if status <> "success" then
          let err = reader.ReadBytes(Int32.MaxValue)
          failwith (Text.Encoding.UTF8.GetString err)

        use fs = IO.File.Open("temp_" + string upd, IO.FileMode.CreateNew, IO.FileAccess.ReadWrite)
        let buf : byte[] = Array.zeroCreate 4096
        let mutable read = 0
        while true do
          read <- reader.Read(buf, 0, buf.Length)
          do! fs.WriteAsync(buf, 0, read);
        
        fs.Position <- 0L
        let md5sum =
          use md5 = Security.Cryptography.MD5.Create()
          fs |> md5.ComputeHash  |> Array.map (fun i -> i.ToString("X2")) |> Array.reduce (+)
        
        use uploadContent = new Http.StreamContent(fs)
        
        let ecsUri = cfg.EscUriPrefix upd.Name upd.Version

        let! response = uploader.PutAsync(ecsUri, uploadContent) |> Async.AwaitTask

        response.EnsureSuccessStatusCode |> ignore
        
        return! loop us ((upd, (ecsUri, md5sum)) :: escInfo)  
      }

      // in
      //   loop upds []
      //   |> Async.Catch
      //   |> Async.RunSynchronously
      //   |> Result.ofChoice
      //   <?> string
    
    let validateUpdateAndFind (db: IDbContext) name =
      validateUpdate name
      >=> db.GetUpdate
      >> Result.map (fst >> fun upd -> (upd, upd |> Ok |> ValidationMessage))
      >> Result.mapError (Error >> ValidationMessage)

    let availableUpdates (db: IDbContext) (escRepository: EscRepository) userId =
      userId
      |> db.GetAvailableUpdates
      <?> UnexpectedError
      <!> Set.filter (function {Name=MApteka} -> false | _ -> true)
      >>= (fun upds ->
          upds
          |> Seq.map (fun upd ->
              upd
              |> escRepository.Get
              <?> UnexpectedError
              >>= Result.ofOption (upd |> EscNotFound |> AvailableMessage)
          )
          |> Seq.toList
          |> Result.sequence
          <!> (fun efis -> efis, efis |> ListAvailable |> AvailableMessage)
      )     

    let convertToEsc (user: CustomerId) (db: IDbContext) cfg (escRepository: EscRepository) =
      db.GetUpdateUri
      >=> uploadEsc cfg user
      >> Result.map (List.map (fun (upd, (escsrc, md5)) -> escRepository.Put upd (escsrc, md5)))
      >=> Result.sequence

    let prepareToInstall (user: CustomerId) (db: IDbContext) (upds: Update seq) =
      user
      |> List.replicate (upds |> Seq.length)
      |> Seq.allPairs upds
      |> Map.ofSeq
      |> db.AddToUsers

  let internal interpretAsWebPart
    (cfg: Config)
    (srv: Services)
    (program: UpdaterProgram<'a>) =
    
    let db = srv.Db // too often use  
  
    let inline fieldOrEmpty name (req: HttpRequest) =
      match req.fieldData name with Choice1Of2 name_ -> name_ | _ -> ""

    let rec nextWebPart (state: HttpInterpreterState) program : WebPart =
      let goAheadIfSucceed next =
        keepGoingIfSucceed state next nextWebPart
      
      match program with
      | OrElse (p1, p2) ->
          [p1; p2] |> List.map (nextWebPart state) |> choose
      
      | Stop _ ->
          state.Message |> classifyDomMessage
          
      | KeepGoing (Authorize next) ->
          request (fun req ->
            match req.header "X-CustomerId" with // temporary unless auth
            | Choice1Of2 user ->
                nextWebPart state (next (Issuer user))

            | _ ->
                UNAUTHORIZED "You should set 'X-CustomerId' header." 
          )

      | KeepGoing (ReadUser next) ->
          pathScan "/updates/%s/%s/users/%s" (fun (_,_,userId) -> nextWebPart state (next userId))

      | KeepGoing (ReadSpecs next) ->
          request (fun req ->
            let specs =
              { Author        = req |> fieldOrEmpty "Author" 
                Summary       = req |> fieldOrEmpty "Summary"
                Description   = req |> fieldOrEmpty "Description"
                ReleaseNotes  = "" // todo
                Created       = DateTime.UtcNow
              }
            in
              nextWebPart state (next specs)
          )
      
      | KeepGoing (ValidateUpdate next) ->
          choose
            [ POST >=>
                pathScan "/updates/%s/%s" (fun (name, vers) ->
                  request (
                    fieldOrEmpty "Constraints"
                    >> (fun c -> c.Split('\n') |> Seq.toList)
                    >> validateUpdateAndConstrs name vers
                    >> goAheadIfSucceed next
                  )
                )
              
              PUT >=>
                pathScan "/updates/%s/%s/users/%s" (fun (name, vers,_) ->
                  validateUpdateAndFind db name vers 
                  |> goAheadIfSucceed next
                )
            ]

      | KeepGoing (CheckVersion (upd, next)) ->
          upd
          |> checkVersion db
          |> goAheadIfSucceed next

      | KeepGoing (ResolveDependencies (upds, next)) ->
          resolveDependencies db upds
          |> goAheadIfSucceed next

      | KeepGoing (Publish ((upd, specs), next)) ->
          request (fun req ->
            req.files
            |> List.tryHead
            |> Option.toChoice (upd |> MissingFileBody |> PublishMessage)
            |> Result.ofChoice
            |> Result.bind (fun file ->
                use fs = IO.File.OpenRead file.tempFilePath
                db.Upsert(upd, specs, fs)
                |> Result.mapError UnexpectedError
            )
            |> Result.map (fun u -> ((u, specs), PublishMessage (Published u)))
            |> goAheadIfSucceed next
          )

      | KeepGoing (GetAvailableUpdates (user, next)) ->
          choose
            [ POST >=> choose
                [ path "/updates/available" >=> (
                    let (Issuer customerId) = user
                    in
                      customerId
                      |> availableUpdates db srv.EscRepository
                      |> goAheadIfSucceed next
                  )
                ]
            ]

      | KeepGoing (ConvertToEsc (user, upds, next)) ->
          convertToEsc user db cfg srv.EscRepository upds
          |> internalServerErrorIfFails (List.map snd >> (fun efis ->
              let jmsg =
                List.map Rep.escFileInfoToJson efis
              
              nextWebPart (setJsonMessage jmsg state) (next efis)
          ))
      
      | KeepGoing (PrepareToInstall (user, upds, next)) ->
          prepareToInstall user db upds
          |> internalServerErrorIfFails (fun _ -> nextWebPart state next)


    in // init state
      nextWebPart {Message=None} program

  let interpretProgram
    (config: Config)
    (srv: Services)
    (cts: System.Threading.CancellationTokenSource)
    program =

    let webpart =
      choose
        [ POST >=> path "auth" >=> OK (string Guid.Empty) // stub for test
          interpretAsWebPart config srv program
        ]
    
    let serverConfig =
      { defaultConfig
          with  cancellationToken = cts.Token
                bindings = [ HttpBinding.create HTTP config.IP config.Port ]
      }

    let _,server =
      startWebServerAsync serverConfig webpart
    
    Async.Start(server, cts.Token)

