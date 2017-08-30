namespace MAptekaGet

[<RequireQualifiedAccess>]
module App =
  open System
  open System.Collections.Generic
  open System.Net
  open System.Threading.Tasks
  open FSharp.Control

  open Suave
  open Suave.Http
  open Suave.Headers
  open Suave.Utils
  open Suave.Operators
  open Suave.Filters
  open Suave.Successful
  open Suave.RequestErrors
  open Suave.Logging
  open Suave.Logging.Message

  open DataAccess
  
  module DR   = DependencyResolution
  module Rep  = Reporting
  module UP   = UpdaterProgram

  type NEL<'a> = NonEmptyList<'a>

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
    { UpdRepository : UpdRepository
      EscRepository : EscRepository
      Logger        : Logger
    }

  type private HttpInterpreterState =
    { Message     : DomainMessage
    }

  /// classify domain message as WebPart
  let classifyDomMessage dmsg =
    match dmsg with
    | AvailableMessage (ListAvailable _) ->
        OK (string dmsg) >=> Writers.setMimeType "application/json"
    
    | Never
    | ReadUpdateMessage (ValidUpdates _)
    | VersionCheckMessage (CorrectVersion _)
    | ResolutionMessage (Solution _)
    | ConvertToEscMessage (Converted _) | ConvertToEscMessage EmptyUpdateList
    | PublishMessage (Published _)
    | AcceptDownloadingMessage (DownloadAccepted _) ->
        OK (string dmsg) >=> Writers.setMimeType "text/plain"

    | AcceptDownloadingMessage (AcceptingEscNotFound _) ->
        NOT_FOUND (string dmsg) >=> Writers.setMimeType "text/plain"

    | VersionCheckMessage _
    | ResolutionMessage _
    | ReadUpdateMessage _
    | PublishMessage _
    | ConvertToEscMessage _
    | AcceptDownloadingMessage _
    | AvailableMessage _ ->
        BAD_REQUEST (string dmsg)

    | UnexpectedError err -> // Internal Server Error
        Response.response HTTP_500 (err |> Text.Encoding.UTF8.GetBytes)
        
  [<AutoOpen>]
  module private Helpers =

    open Parsing
    open Utils.Parsing
    open Choice.Infixes
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

      let downloadEsc (updUris: NEL<Uri>) (escConvertUri: Uri) customerId =
        async {
          use downloader = new Http.HttpClient()

          use formContent =
            new Http.FormUrlEncodedContent
              (seq {
                yield KeyValuePair("MailParticipantID", customerId)
                yield KeyValuePair("StorageUrl", updUris.Head.Host)
                for uri in updUris do
                  yield KeyValuePair("PathUPD", uri.PathAndQuery)
              })
          
          let! response =
            downloader.PostAsync(escConvertUri, formContent) |> Async.AwaitTask
            
          let! responseStream =
            response.Content.ReadAsStreamAsync() |> Async.AwaitTask

          let! status = responseStream.AsyncRead 7
          
          if (status |> Array.map char |> System.String |> String.toLowerInvariant) <> "success" then
            use errms = new IO.MemoryStream()
            do! IO.copyData responseStream errms;
            failwith (Text.Encoding.GetEncoding(1251).GetString (errms.ToArray()))

          return responseStream
      }

    let inline thenIfSucceed (state: HttpInterpreterState) next f = function
      | Right (x, dmsg) ->
          f ({state with Message = dmsg}) (next x)
      | Left dmsg ->
          classifyDomMessage dmsg

    let inline thenIfSucceedAsync (state: HttpInterpreterState) next f input ctx =
      async {
        let! res = input
        return! thenIfSucceed state next f res ctx
      }

    let rec addConstraintRes<'err> (upd: Choice<Update, 'err>) (constrs: Constraint list) =
      match constrs with
      | []          -> upd
      | constr::cs  -> addConstraintRes (upd <!> addConstraint constr) cs
    
    let private validateUpdate name vers =
      update <-- (name + "-" + vers)

    let validateUpdateAndConstrs name vers constrs =  
      constrs
      |> List.map ((<--) dependency)
      |> Choice.sequence
      <?> InvalidConstraints
      >>= addConstraintRes (validateUpdate name vers <?> InvalidUpdate)
      <!> (fun upd -> (upd, Set.singleton upd |> ValidUpdates |> ReadUpdateMessage))
      <?> ReadUpdateMessage

    let checkVersion (db: UpdRepository) (logger: Logger) (upd: Update) =
      async {
        let! upds = db.GetVersionsByName upd.Name

        logger.debug (
          eventX "Versions of {update}: {versions}"
            >> setField "update" upd
            >> setField "versions" upds)

        let dmsg = VersionCheckMessage 
        let checkResult = checkUpdateVersion upds upd
        match checkResult with
        | CorrectVersion _ ->
            return Right (upd, dmsg checkResult)
        
        | _ ->
            return Left (dmsg checkResult)
      }

    let resolveDependencies (db: UpdRepository) (logger: Logger) (updates: Update seq) =
      async {
        let dmsg = ResolutionMessage
        match Seq.toList updates with
        | [] ->
          return Right ([], dmsg (Solution []))

        | u::us ->
          let! res = DR.resolve db.GetVersionsByName (NEL.create u us)

          logger.debug (
            eventX "Dependencies of {updates}: {deps}"
              >> setField "updates" (sprintf "%A" updates)
              >> setField "deps" (sprintf "%A" res))

          match res with
          | Solution forest as sol ->
              return Right (forest, dmsg sol)

          | UpdateNotFound (updName, _) ->
              let! nearbyNames = db.NearbyNames updName
              match nearbyNames with
              | Right ns -> return Left ^ dmsg ^ UpdateNotFound (updName, Seq.toList ns)
              | Left err -> return Left ^ UnexpectedError err
          
          | res ->
              return Left (dmsg res)
      }

    let private setMessage msg state =
      {state with Message = msg}

    let convertToEsc
      (db: UpdRepository)
      (escRepository: EscRepository)
      (escConvertUri: Uri)
      customerId upds externalHost =
      
      async {
        let updSet = Set.ofSeq upds
        let! eitherAvailUpds = db.GetAvailableUpdates customerId

        match eitherAvailUpds with
        | Left err ->
          return Left ^ UnexpectedError err
        
        | Right availUpds ->
          let diff = Set.difference updSet availUpds
          if not (Set.isEmpty diff) then
            return diff |> DependsOnUnavailable |> ConvertToEscMessage |> Left
          else
            let! maybeUpdInfo =
              upds
              |> Seq.mapi (fun ord upd -> db.GetUpdateUri upd externalHost |> Async.map (fun x -> ord, x))
              |> Async.Parallel
              |> Async.map (Seq.sortBy fst >> Seq.map snd)
            
            match maybeUpdInfo |> Option.sequence |> Option.map Seq.toList with
            | None ->
                return updSet |> ConvertionSourceNotFound |> ConvertToEscMessage |> Left

            | Some [] ->
                return EmptyUpdateList |> ConvertToEscMessage |> Left
            
            | Some (updUri :: rest) ->  
                if not (IO.Directory.Exists TempDir) then
                  IO.Directory.CreateDirectory TempDir |> ignore

                let tempPath = IO.Path.Combine(TempDir, sprintf "%O.esc" (Guid.NewGuid()))
                try
                  use! stream =
                    IOHelpers.downloadEsc (NEL.create updUri rest) escConvertUri customerId
                  
                  let tempZipPath = tempPath + ".zip"
                  let entryName = (IO.FileInfo tempPath).Name

                  IOHelpers.clearFileTemp tempPath;

                  do! IO.compressFileToZip entryName stream tempZipPath

                  use zipStream = IO.File.OpenRead tempZipPath
                  let! eitherEid = escRepository.Create customerId updSet (zipStream :> IO.Stream)

                  return
                    eitherEid
                    |> Choice.map (fun eid ->
                      let escUri = escRepository.GetDownloadLink eid externalHost
                      in
                        (escUri, eid), (updSet, eid) |> Converted |> ConvertToEscMessage
                    )
                    |> Choice.mapSnd UnexpectedError
                
                finally
                  IOHelpers.clearFileTemp tempPath
      }
    
    let readUserUpdates (db: UpdRepository) (form: (string * string option) list) user =
      form
      |> List.choose snd
      |> Set.ofList
      |> Seq.map (fun str ->
        (update <-- str)
        <!> db.HeadUpdate
        <!> Async.map (Choice.bind ^ Choice.ofOption (sprintf "Update '%s' not found" str))
        |> Choice.revAsync
        |> Async.map (Choice.bind id)
      )
      |> Async.Parallel
      |> Async.map (
          Seq.toList
          >> Choice.sequence
          >> Choice.map (Seq.map fst >> Set.ofSeq >> fun upds -> ((upds, user), upds |> ValidUpdates |> ReadUpdateMessage))
          >> Choice.mapSnd (ReadUpdateMessage << InvalidUpdate)
      )

    let getInstalledUpdatesFromProtocol (db: UpdRepository) =
      IOHelpers.extractProtocolFromRequest
      >> Async.map (IOHelpers.extractUniqueCodesFromProtocol >> Seq.toList)

    let availableUpdates (db: UpdRepository) externalUri (escRepository: EscRepository) (compressedProtocol: byte[] option) userId =
      let getFromDb () =
        escRepository.Head userId
        |> Async.map ^ Choice.mapSnd ^ UnexpectedError
        |> Async.map ^ Choice.bind (
            Seq.map (fun (eid, (upds,_)) ->
              let link = escRepository.GetDownloadLink eid externalUri
              ((link, eid), upds)
            )
            >> Seq.toList
            >> (fun lst -> Right (lst, lst |> List.map fst |> ListAvailable |> AvailableMessage))
        )
      in
        compressedProtocol
        |> Option.map (
          getInstalledUpdatesFromProtocol db
          >> Async.bind (db.AddUpdatesByUniqueCode userId)
          >> Async.bind (ignore >> getFromDb)
        )
        |> Option.defaultValue (getFromDb ())

    let prepareToInstall (user: CustomerId) (db: UpdRepository) (upds: Update seq) =
      user
      |> List.replicate (Seq.length upds)
      |> Seq.zip upds
      |> Map.ofSeq
      |> db.AddToUsers

    let internal acceptDownloading (escRepository: EscRepository) (eid: EscId) user =
      user
      |> escRepository.Head
      |> Async.map ^ Choice.mapSnd UnexpectedError
      |> Async.bind ^ Choice.bindAsync (
          Seq.filter (fst >> ((=) eid))
          >> Seq.collect (fun (_,(upds,_)) -> upds)
          >> Set.ofSeq
          >> (fun upds ->
            if Set.isEmpty upds then
              eid
              |> AcceptingEscNotFound 
              |> AcceptDownloadingMessage
              |> Left
              |> Async.result
            else
              escRepository.AcceptDownloading eid
              |> Async.map ^ Choice.mapSnd UnexpectedError
              |> Async.map ^ Choice.map (fun () -> (), eid |> DownloadAccepted |> AcceptDownloadingMessage)
          )
      )
  
  let internal interpretAsWebPart
    (cfg: Config)
    (srv: Services)
    (logger: Logger)
    (program: UpdaterProgram<'a>) =
    
    let db = srv.UpdRepository // too often use  
  
    let inline fieldOrEmpty name (req: HttpRequest) =
      match req.fieldData name with Choice1Of2 name_ -> name_ | _ -> ""

    let inline externalHost (req: HttpRequest) =
      sprintf "%s://%s" cfg.EscExternalScheme req.clientHostTrustProxy

    let rec nextWebPart (state: HttpInterpreterState) program : WebPart =
      let inline keepGoingIfSucceed next =
        thenIfSucceed state next nextWebPart

      let inline keepGoingIfSucceedAsync next =
        thenIfSucceedAsync state next nextWebPart

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
                |> keepGoingIfSucceedAsync next
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
      
      | AndThen (ReadUpdate next) ->
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
          |> checkVersion db logger
          |> keepGoingIfSucceedAsync next

      | AndThen (ResolveDependencies (upds, next)) ->
          resolveDependencies db logger upds
          |> keepGoingIfSucceedAsync next

      | AndThen (Publish ((upd, specs), next)) ->
          request (fun req ->
            req.files
            |> List.tryHead
            |> Option.toChoice (upd |> MissingFileBody |> PublishMessage)
            |> Choice.map (fun file ->
                db.Upsert upd specs (IO.FileInfo file.tempFilePath)
                |> Async.map ^ Choice.mapSnd UnexpectedError
            )
            |> Choice.revAsync
            |> Async.map ^ Choice.bind id
            |> Async.map ^ Choice.map (fun () -> ((upd, specs), PublishMessage (Published upd)))
            |> keepGoingIfSucceedAsync next
          )

      | AndThen (GetAvailableUpdates (user, next)) ->
          let handle reqToBody =
            request (fun req ->
              let (Issuer customerId) = user
              let externalUri = req |> externalHost |> sprintf "%s/api/v1/" |> Uri
              in
                customerId
                |> availableUpdates db externalUri srv.EscRepository (reqToBody req)
                |> keepGoingIfSucceedAsync next
            )

          path "/api/v1/updates/available" >=>
            choose
              [ GET >=> handle (fun req -> Some req.rawForm)                
                POST >=> handle (fun _ -> None) 
              ]

      | AndThen (ConvertToEsc (user, upds, next)) ->
          request (fun req ->
            convertToEsc db srv.EscRepository cfg.EscConvertUri user upds (req |> externalHost |> Uri)
            |> keepGoingIfSucceedAsync next
          )
      
      | AndThen (PrepareToInstall (user, upds, next)) -> fun ctx ->
          prepareToInstall user db upds
          |> Async.bind (function
            | Right _   -> nextWebPart state next ctx
            | Left err  -> failwith err
          )
          
      
      | AndThen (AcceptDownloading (user, uri, next)) ->
          acceptDownloading srv.EscRepository uri user
          |> keepGoingIfSucceedAsync (fun _ -> next)

    in // init state
      nextWebPart {Message=Never} program

  let interpretProgram (config: Config) (srv: Services) program =    
    let webpart =
      choose
        [ POST >=> path "/updates/auth" >=> OK (string Guid.Empty) // stub for test
          interpretAsWebPart config srv srv.Logger program
        ] >=>
          logWithLevelStructured Logging.Debug srv.Logger (fun context ->
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
          with  bindings = [ HttpBinding.create HTTP config.IP config.Port ]
                logger = srv.Logger
      }

    startWebServer serverConfig webpart
    
