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
      EscUriPrefix  : Uri
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
    | ConvertToEscMessage (Converted _) 
    | PublishMessage (Published _) ->
        OK (string dmsg) >=> Writers.setMimeType "text/plain"

    | VersionCheckMessage _
    | ResolutionMessage _
    | ValidationMessage _
    | PublishMessage _
    | ConvertToEscMessage (ConvertionSourceNotFound _)
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

    let [<Literal>] TempDir = "__temp__"

    module internal IOHelpers =

      open System.Xml.Linq

      let extractUniqueCodesFromProtocol (input: IO.Stream) =
  
        Text.Encoding.RegisterProvider(Text.CodePagesEncodingProvider.Instance);

        let xdoc = XDocument.Load (input)
        in "Item" |> XName.Get |> xdoc.Descendants |> Seq.map (fun el -> el.Value)

      let extractProtocolFromRequest (bytes: byte[]) = async {
        let compressedStream = new IO.MemoryStream(bytes)
        let decompressedStream = new IO.MemoryStream()
        use decompressionStream =
          new IO.Compression.GZipStream(compressedStream, IO.Compression.CompressionMode.Decompress)
        do! IO.copyData compressedStream decompressedStream

        decompressedStream.Position <- 0L;

        return decompressedStream
      }

      let clearFileTemp path =
        if IO.File.Exists path then
          IO.File.Delete path;
          IO.File.Delete (path + ".zip")
        else
          ()

      let downloadEsc (updUri: Uri) (escConvertUri: Uri) customerId tempFilePath = async {
        use downloader = new Http.HttpClient()
        
        use formContent =
          new Http.FormUrlEncodedContent
            ([  KeyValuePair("MailParticipantID", customerId)
                KeyValuePair("StorageUrl", updUri.Host)
                KeyValuePair("DirResult", "esc")
                KeyValuePair("PathUPD", updUri.PathAndQuery)
            ])
        
        let! response =
          downloader.PostAsync(escConvertUri, formContent) |> Async.AwaitTask
        let! responseStream =
          response.Content.ReadAsStreamAsync() |> Async.AwaitTask

        let! status = responseStream.AsyncRead 7
        
        if (status |> Array.map char |> System.String |> String.toLowerInvariant) <> "success" then
          let! err = responseStream.AsyncRead Int32.MaxValue // to the end
          failwith (Text.Encoding.UTF8.GetString err)
        
        clearFileTemp tempFilePath;

        let tmpFileStream =
          IO.File.Open(tempFilePath, IO.FileMode.CreateNew, IO.FileAccess.ReadWrite)

        do! IO.copyData responseStream tmpFileStream;

        tmpFileStream.Position <- 0L;

        return tmpFileStream
      }

      let uploadEsc (ecsUri: Uri) tempZipPath = async {
        use uploader = new Http.HttpClient()
        use tmpZipFileStream = IO.File.OpenRead tempZipPath
        use uploadContent = new Http.StreamContent(tmpZipFileStream)
        
        let! response = uploader.PutAsync(ecsUri, uploadContent) |> Async.AwaitTask
        response.EnsureSuccessStatusCode |> ignore
      }

    
    
    let inline keepGoingIfSucceed (state: HttpInterpreterState) next f = function
      | Ok (x, dmsg) ->
          f ({state with Message = dmsg}) (next x)
      | Error dmsg ->
          classifyDomMessage dmsg

    let inline keepGoingIfSucceedAsync (state: HttpInterpreterState) next f input ctx =
      async {
        let! res = input
        return! keepGoingIfSucceed state next f res ctx
      }

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
        if not (IO.Directory.Exists TempDir) then
          IO.Directory.CreateDirectory TempDir |> ignore

        let tempPath = IO.Path.Combine(TempDir, string upd + ".esc")
        
        try
          use! tmpFileStream = IOHelpers.downloadEsc uri cfg.EscConvertUri customerId tempPath

          let tempZipPath = tempPath + ".zip"
          let entryName = (IO.FileInfo tempPath).Name

          do! IO.compressFileToZip entryName tmpFileStream tempZipPath

          let md5sum =
            use zipFileStream = IO.File.OpenRead tempZipPath
            IO.calculateMd5 zipFileStream

          let ub = UriBuilder(cfg.EscUriPrefix)
          ub.Path <- (sprintf "%O-%O" upd.Name upd.Version)

          let ecsUri = ub.Uri

          do! IOHelpers.uploadEsc ecsUri tempZipPath

          return (upd, (ecsUri, md5sum))

        finally
          IOHelpers.clearFileTemp tempPath;
      }
      |> Async.Catch
      |> Async.RunSynchronously
      |> Result.ofChoice
      <?> string
      <?> UnexpectedError
    
    let readUpdateAndUser (db: IDbContext) name vers user =
      validateUpdate name vers
      >>= db.GetUpdate
      <!> (fst >> fun upd -> ((upd, user), upd |> Ok |> ValidationMessage))
      <?> (Error >> ValidationMessage)

    let getInstalledUpdatesFromProtocol (db: IDbContext) (data: byte[]) =
      async {
        let! protocol = IOHelpers.extractProtocolFromRequest data
        let uniqueCodes = IOHelpers.extractUniqueCodesFromProtocol protocol
        return Seq.toList uniqueCodes
      }

    let availableUpdates (db: IDbContext) (escRepository: EscRepository) (compressedProtocol: byte[] option) userId =
      let getFromDb () =
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
      
      async {
        match compressedProtocol with
        | Some bytes ->
          let! uniqueCodes =
            getInstalledUpdatesFromProtocol db bytes

          let! res =
            db.AddUpdatesByUniqueCode (uniqueCodes, userId)

          return
            match res with
            | Ok () ->
                getFromDb () 
            | Error err ->
                Error (UnexpectedError err)

        | None ->
          return getFromDb ()
      }    

    let convertToEsc (user: CustomerId) (db: IDbContext) cfg (escRepository: EscRepository) upd =
      upd 
      |> db.GetUpdateUri
      <?> (fun _ -> ConvertToEscMessage (ConvertionSourceNotFound upd))
      >>= uploadEsc cfg user
      >>= (fun (upd, (escsrc, md5)) ->
          escRepository.Put upd (escsrc, md5)
          <!> (fun x -> x, x |> Converted |> ConvertToEscMessage)
          <?> UnexpectedError
      )

    let prepareToInstall (user: CustomerId) (db: IDbContext) (upd: Update) =
      (upd, user)
      |> Seq.singleton 
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

      | KeepGoing (ReadUpdateAndUser next) ->
          PUT >=>
            pathScan "/updates/%s/%s/users/%s" (fun (name, vers, userId) ->
              readUpdateAndUser db name vers userId
              |> goAheadIfSucceed next
            )

      | KeepGoing (ReadSpecs next) ->
          request (fun req ->
            let specs =
              { Author        = req |> fieldOrEmpty "Author" 
                Summary       = req |> fieldOrEmpty "Summary"
                UniqueCode    = req |> fieldOrEmpty "UniqueCode"
                Description   = req |> fieldOrEmpty "Description"
                ReleaseNotes  = "" // todo
                Created       = DateTime.UtcNow
              }
            in
              nextWebPart state (next specs)
          )
      
      | KeepGoing (ValidateUpdate next) ->
          POST >=>
            pathScan "/updates/%s/%s" (fun (name, vers) ->
              request (
                fieldOrEmpty "Constraints"
                >> (fun c -> c.Split('\n') |> Seq.toList)
                >> validateUpdateAndConstrs name vers
                >> goAheadIfSucceed next
              )
            )

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
          let handle rawBody =
            let (Issuer customerId) = user
            in
              customerId
              |> availableUpdates db srv.EscRepository rawBody
              |> keepGoingIfSucceedAsync state next nextWebPart

          path "/updates/available" >=> choose
            [ GET >=> handle None
              
              POST >=> request (fun req -> handle (Some req.rawForm))
            ]

      | KeepGoing (ConvertToEsc (user, upd, next)) ->
          convertToEsc user db cfg srv.EscRepository upd
          |> goAheadIfSucceed next
      
      | KeepGoing (PrepareToInstall (user, upd, next)) ->
          match prepareToInstall user db upd with
          | Ok _ -> // todo handle accurately
              nextWebPart state next
          | Error err ->
              failwith err

    in // init state
      nextWebPart {Message=Never} program

  open Suave.Logging

  let interpretProgram
    (config: Config)
    (srv: Services)
    (cts: System.Threading.CancellationTokenSource)
    program =

    let webpart =
      choose
        [ POST >=> path "/updates/auth" >=> OK (string Guid.Empty) // stub for test
          interpretAsWebPart config srv program
        ]
    
    let serverConfig =
      { defaultConfig
          with  cancellationToken = cts.Token
                bindings = [ HttpBinding.create HTTP config.IP config.Port ]
                logger = LiterateConsoleTarget([||], LogLevel.Debug)
      }

    let _,server =
      startWebServerAsync serverConfig webpart
    
    Async.Start(server, cts.Token)

