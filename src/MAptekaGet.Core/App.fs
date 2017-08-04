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
  open Suave.Utils
  open Suave.Operators
  open Suave.Filters
  open Suave.Successful
  open Suave.RequestErrors
  
  open DataAccess
  
  module NEL  = NonEmptyList
  module DR   = DependencyResolution
  module Rep  = Reporting
  module UP   = UpdaterProgram

  Text.Encoding.RegisterProvider(Text.CodePagesEncodingProvider.Instance); // we need win-1251 sometimes...

  type Config =
    { IP            : Net.IPAddress
      Port          : Sockets.Port
      StaticBaseUri : Uri
      EscConvertUri : Uri
      EscExternalScheme
                    : string
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
    | ConvertToEscMessage (Converted _) | ConvertToEscMessage EmptyUpdateList
    | PublishMessage (Published _)
    | AcceptDownloadingMessage (Ok _) ->
        OK (string dmsg) >=> Writers.setMimeType "text/plain"

    | VersionCheckMessage _
    | ResolutionMessage _
    | ValidationMessage _
    | PublishMessage _
    | ConvertToEscMessage (ConvertionSourceNotFound _)
    | AcceptDownloadingMessage _
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

    let TempDir = env "ESC_TEMP_DIR" |> Choice.orDefault "__temp__"

    module internal IOHelpers =

      open System.Xml.Linq

      let extractUniqueCodesFromProtocol (input: IO.Stream) =

        let xdoc = XDocument.Load (input)
        in "Item" |> XName.Get |> xdoc.Descendants |> Seq.map (fun el -> el.Value)

      let extractProtocolFromRequest (bytes: byte[]) = async {
        let compressedStream = new IO.MemoryStream(bytes)
        let decompressedStream = new IO.MemoryStream()
        use decompressionStream =
          new IO.Compression.GZipStream(compressedStream, IO.Compression.CompressionMode.Decompress)
        do! IO.copyData decompressionStream decompressedStream

        decompressedStream.Position <- 0L;

        return decompressedStream
      }

      let clearFileTemp path =
        if IO.File.Exists path then
          IO.File.Delete path;
          IO.File.Delete (path + ".zip")
        else
          ()

      let downloadEsc (updUris: Uri list) (escConvertUri: Uri) customerId tempFilePath = async {
        match updUris with
        | [] ->
          return None

        | updUri :: _ ->
          use downloader = new Http.HttpClient()

          printfn "Host: %A" updUri.Host;
          printfn "PathAndQuery: %A" updUri.PathAndQuery;

          use formContent =
            new Http.FormUrlEncodedContent
              ([  KeyValuePair("MailParticipantID", customerId)
                  KeyValuePair("StorageUrl", updUri.Host)
                  // KeyValuePair("PathUPD", updUri.PathAndQuery)
              ] @ (updUris |> List.map (fun uri -> KeyValuePair("PathUPD", uri.PathAndQuery)))
              )
          
          let! response =
            downloader.PostAsync(escConvertUri, formContent) |> Async.AwaitTask
            
          let! responseStream =
            response.Content.ReadAsStreamAsync() |> Async.AwaitTask

          let! status = responseStream.AsyncRead 7
          
          if (status |> Array.map char |> System.String |> String.toLowerInvariant) <> "success" then
            use errms = new IO.MemoryStream()
            do! IO.copyData responseStream errms;
            failwith (Text.Encoding.GetEncoding(1251).GetString (errms.ToArray()))
          
          clearFileTemp tempFilePath;

          let mutable maybeStream : IO.Stream option = None
          try
            let fs = IO.File.Open(tempFilePath, IO.FileMode.CreateNew, IO.FileAccess.ReadWrite)
            maybeStream <- Some (fs :> IO.Stream)
            do! IO.copyData responseStream fs;
            fs.Position <- 0L;
            return maybeStream
          with _ ->
            do
              match maybeStream with Some stream -> stream.Dispose(); | _ -> ();
            return None
      }

      let uploadEsc (ecsUri: Uri) tempZipPath = async {
        use uploader = new Http.HttpClient()
        use tmpZipFileStream = IO.File.OpenRead tempZipPath
        use uploadContent = new Http.StreamContent(tmpZipFileStream)
        
        let! response = uploader.PutAsync(ecsUri, uploadContent) |> Async.AwaitTask
        response.EnsureSuccessStatusCode |> ignore
      }

    let inline thenIfSucceed (state: HttpInterpreterState) next f = function
      | Ok (x, dmsg) ->
          f ({state with Message = dmsg}) (next x)
      | Error dmsg ->
          classifyDomMessage dmsg

    let inline thenIfSucceedAsync (state: HttpInterpreterState) next f input ctx =
      async {
        let! res = input
        return! thenIfSucceed state next f res ctx
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
      <!> (fun upd -> (upd, Set.singleton upd |> Ok |> ValidationMessage))
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

    let uploadEsc (cfg: Config) customerId uris =
      
      async {
        if not (IO.Directory.Exists TempDir) then
          IO.Directory.CreateDirectory TempDir |> ignore

        let tempPath = IO.Path.Combine(TempDir, sprintf "%O.esc" (Guid.NewGuid()))

        try
          let! maybeStream =
            IOHelpers.downloadEsc uris cfg.EscConvertUri customerId tempPath

          match maybeStream with
          | Some stream -> 
              use tmpFileStream = stream 
              let tempZipPath = tempPath + ".zip"
              let entryName = (IO.FileInfo tempPath).Name

              do! IO.compressFileToZip entryName tmpFileStream tempZipPath

              let md5sum =
                use zipFileStream = IO.File.OpenRead tempZipPath
                (IO.calculateMd5 zipFileStream).ToUpper()

              let ub = UriBuilder(cfg.StaticBaseUri)
              ub.Path <- ub.Path + "esc/" + md5sum + ".esc"

              do! IOHelpers.uploadEsc ub.Uri tempZipPath

              return Guid md5sum

          | None ->
            return failwith "IOHelpers.downloadEsc returns None.";

        finally
          IOHelpers.clearFileTemp tempPath;
      }
      |> Async.Catch
      |> Async.RunSynchronously
      |> Result.ofChoice
      <?> string
      <?> UnexpectedError
    
    let readUserUpdates (db: IDbContext) (form: (string * string option) list) user =
      form
      |> List.choose snd
      |> Set.ofList
      |> Seq.map (fun str -> (update <-- str) >>= db.GetUpdate)
      |> Seq.toList
      |> Result.sequence
      <!> (List.map fst >> Set.ofList >> fun upds -> ((upds, user), upds |> Ok |> ValidationMessage))
      <?> (Error >> ValidationMessage)

    let getInstalledUpdatesFromProtocol (db: IDbContext) (data: byte[]) =
      async {
        let! protocol = IOHelpers.extractProtocolFromRequest data
        let uniqueCodes = IOHelpers.extractUniqueCodesFromProtocol protocol
        return Seq.toList uniqueCodes
      }

    let availableUpdates (db: IDbContext) externalHost (escRepository: EscRepository) (compressedProtocol: byte[] option) userId =
      let getFromDb () =
        userId
        |> escRepository.Get
        <?> UnexpectedError
        <!> (fun res ->
          let lst =
            res
            |> List.ofSeq
            |> List.map (fun (eid,_,_) -> Uri (externalHost + eid.ToString("N") + ".esc"), eid)
          in
            lst, lst |> ListAvailable |> AvailableMessage
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

    let convertToEsc (user: CustomerId) (db: IDbContext) (cfg: Config) (escRepository: EscRepository) upds =
      printfn "%A" host;
      
      if Set.isEmpty upds then
        Ok (None, ConvertToEscMessage EmptyUpdateList)
      else
        upds
        |> Seq.map (db.GetUpdateUri cfg.StaticBaseUri)
        |> Seq.toList
        |> Result.sequence
        <?> (fun _ -> upds |> ConvertionSourceNotFound |> ConvertToEscMessage)
        >>= (fun res ->
          let res =
            List.filter (function ({Name=MApteka},_) -> false | _ -> true) res

          let updSet =
            res |> List.map fst |> Set.ofList

          uploadEsc cfg user (List.map snd res)
          >>= (fun md5 ->
            escRepository.Put user md5 updSet false
            <!> (fun efi -> Some efi, (upds, efi) |> Converted |> ConvertToEscMessage)
            <?> UnexpectedError
          )
        )

    let prepareToInstall (user: CustomerId) (db: IDbContext) (upds: Update seq) =
      user
      |> List.replicate (Seq.length upds)
      |> Seq.zip upds
      |> Map.ofSeq
      |> db.AddToUsers

    let internal acceptDownloading (escRepository: EscRepository) (escId: EscId) userId =
      userId
      |> escRepository.Get
      <!> Seq.collect (fun (eid, updSet,_) ->
          let set' =
            if eid = escId
              then updSet
              else Set.empty
          
          eid
          |> Seq.replicate (Seq.length set')
          |> Seq.zip set'
      )
      <!> Seq.toList
      >>= (function
        | [] -> Ok ()
        | ((_, efi) :: _) as ins ->
            escRepository.Put userId efi (ins |> List.map fst |> Set.ofList) true <!> ignore
      )
      <!> (fun _ -> ((), escId |> Ok |> AcceptDownloadingMessage))
      <?> Error
      <?> ValidationMessage
  
  let internal interpretAsWebPart
    (cfg: Config)
    (srv: Services)
    (program: UpdaterProgram<'a>) =
    
    let db = srv.Db // too often use  
  
    let inline fieldOrEmpty name (req: HttpRequest) =
      match req.fieldData name with Choice1Of2 name_ -> name_ | _ -> ""

    let rec nextWebPart (state: HttpInterpreterState) program : WebPart =
      let inline keepGoingIfSucceed next =
        thenIfSucceed state next nextWebPart

      match program with
      | Stop _ ->
          state.Message |> classifyDomMessage

      | OrElse (p1, p2) ->
          [p1; p2] |> List.map (nextWebPart state) |> choose
          
      | AndThen (Authorize next) ->
          request (fun req ->
            match req.header "X-CustomerId" with // temporary unless auth
            | Choice1Of2 user ->
                nextWebPart state (next (Issuer user))

            | _ ->
                UNAUTHORIZED "You should set 'X-CustomerId' header." 
          )

      | AndThen (ReadUserUpdates next) ->
          PATCH >=>
            pathScan "/api/v1/users/%s/updates" (fun userId ->
              request (fun req ->
                readUserUpdates db req.form userId
                |> keepGoingIfSucceed next
              )
            )

      | AndThen (ReadSpecs next) ->
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

      | AndThen (ReadEscUri next) ->
          PATCH >=>
            pathScan "/api/v1/internal/esc/%s" (Guid >> next >> nextWebPart state)
      
      | AndThen (ValidateUpdate next) ->
          POST >=>
            pathScan "/api/v1/updates/%s/%s" (fun (name, vers) ->
              request (
                fieldOrEmpty "Constraints"
                >> (fun c -> c.Split('\n') |> Seq.toList)
                >> validateUpdateAndConstrs name vers
                >> keepGoingIfSucceed next
              )
            )

      | AndThen (CheckVersion (upd, next)) ->
          upd
          |> checkVersion db
          |> keepGoingIfSucceed next

      | AndThen (ResolveDependencies (upds, next)) ->
          resolveDependencies db upds
          |> keepGoingIfSucceed next

      | AndThen (Publish ((upd, specs), next)) ->
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
            |> keepGoingIfSucceed next
          )

      | AndThen (GetAvailableUpdates (user, next)) ->
          let handle reqToBody =
            request (fun req ->
              let (Issuer customerId) = user
              let externalHost =
                sprintf "%s://%s" cfg.EscExternalScheme req.clientHostTrustProxy
              in
                customerId
                |> availableUpdates db externalHost srv.EscRepository (reqToBody req)
                |> thenIfSucceedAsync state next nextWebPart
            )

          path "/api/v1/updates/available" >=> choose
            [ GET >=> handle (fun _ -> None)
              
              POST >=> handle (fun req -> Some req.rawForm)
            ]

      | AndThen (ConvertToEsc (user, upds, next)) ->
          convertToEsc user db cfg srv.EscRepository upds
          |> keepGoingIfSucceed next
      
      | AndThen (PrepareToInstall (user, upds, next)) ->
          match prepareToInstall user db upds with
          | Ok _ -> // todo handle accurately
              nextWebPart state next
          | Error err ->
              failwith err
      
      | AndThen (AcceptDownloading (user, uri, next)) ->
          acceptDownloading srv.EscRepository uri user
          |> keepGoingIfSucceed (fun _ -> next)

    in // init state
      nextWebPart {Message=Never} program

  open Suave.Logging

  let interpretProgram
    (config: Config)
    (srv: Services)
    (cts: System.Threading.CancellationTokenSource)
    program =

    let logger =
      LiterateConsoleTarget([||], LogLevel.Debug) :> Logger
    
    let webpart =
      choose
        [ POST >=> path "/updates/auth" >=> OK (string Guid.Empty) // stub for test
          interpretAsWebPart config srv program
        ] >=>
          logWithLevelStructured Logging.Info logger (fun context ->
            let fields =
              [ "requestMethod"       , box context.request.method
                "requestPathAndQuery" , box context.request.url.PathAndQuery
                "requestId"           , box context.request.trace.traceId
                "httpStatusReason"    , box context.response.status.reason
                "httpStatusCode"      , box context.response.status.code
                "requestForm"         , box context.request.form
              ]
              |> Map.ofList

            let format = 
              "HTTP {requestMethod} at \"{requestPathAndQuery}\" responded {httpStatusReason} ({httpStatusCode})"
            in
              (format, fields)
        )
    
    let serverConfig =
      { defaultConfig
          with  cancellationToken = cts.Token
                bindings = [ HttpBinding.create HTTP config.IP config.Port ]
                logger = logger
      }

    startWebServer serverConfig webpart
    
