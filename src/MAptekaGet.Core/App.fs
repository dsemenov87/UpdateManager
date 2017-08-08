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
    { UpdRepository : UpdRepository
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

    let checkVersion (db: UpdRepository) (upd: Update) =
      async {
        let! upds = db.GetVersionsByName upd.Name
        let dmsg = VersionCheckMessage 
        let checkResult = cheskUpdateVersion upds upd
        match checkResult with
        | CorrectVersion _ ->
            return Ok (upd, dmsg checkResult)
        
        | _ ->
            return Error (dmsg checkResult)
      }

    let resolveDependencies (db: UpdRepository) (updates: Update seq) =
      async {
        let dmsg = ResolutionMessage
        match Seq.toList updates with
        | [] ->
          return Ok ([], dmsg (Solution []))

        | u::us ->
          let! deps = db.GetDependencies updates

          match DR.resolve deps (NEL.create u us) with
          | Solution forest as sol ->
              return Ok (forest, dmsg sol)
          | res ->
              return Error (dmsg res)
      }

    let private setMessage msg state =
      {state with Message = msg}

    let convertToEsc
      (db: UpdRepository)
      (escRepository: EscRepository)
      (escConvertUri: Uri)
      customerId upds externalHost =
      
      async {
        let! maybeUpdInfo =
          upds
          |> Seq.map (fun upd -> db.GetUpdateUri upd externalHost)
          |> Async.Parallel
        
        match Option.sequence maybeUpdInfo with
        | None ->
            return upds |> ConvertionSourceNotFound |> ConvertToEscMessage |> Error

        | Some s when Seq.isEmpty s ->
            return EmptyUpdateList |> ConvertToEscMessage |> Error
        
        | Some updUris ->
            if not (IO.Directory.Exists TempDir) then
              IO.Directory.CreateDirectory TempDir |> ignore

            let tempPath = IO.Path.Combine(TempDir, sprintf "%O.esc" (Guid.NewGuid()))
            try
              let! maybeStream =
                IOHelpers.downloadEsc (Seq.toList updUris) escConvertUri customerId tempPath

              match maybeStream with
              | Some stream ->  
                let tempZipPath = tempPath + ".zip"
                do!
                  use tmpFileStream = stream 
                  let entryName = (IO.FileInfo tempPath).Name
                  IO.compressFileToZip entryName tmpFileStream tempZipPath

                use zipStream = IO.File.OpenRead tempZipPath
                let! eid = escRepository.Create customerId upds (zipStream :> IO.Stream)

                let escUri = escRepository.GetDownloadLink eid externalHost

                return Ok ((escUri, eid), (upds, eid) |> Converted |> ConvertToEscMessage)

              | None ->
                  return failwith "IOHelpers.downloadEsc returns None.";
            
            finally
              IOHelpers.clearFileTemp tempPath;
          
      }
    
    let readUserUpdates (db: UpdRepository) (form: (string * string option) list) user =
      form
      |> List.choose snd
      |> Set.ofList
      |> Seq.map (fun str ->
        (update <-- str)
        <!> db.HeadUpdate
        <!> Async.map (Result.ofOption (sprintf "Update '%s' not found" str))
        |> Result.mapAsync
        |> Async.map (Result.bind id)
      )
      |> Async.Parallel
      |> Async.map (
          Seq.toList
          >> Result.sequence
          >> Result.map (Seq.map fst >> Set.ofSeq >> fun upds -> ((upds, user), upds |> Ok |> ValidationMessage))
          >> Result.mapError (Error >> ValidationMessage)
      )

    let getInstalledUpdatesFromProtocol (db: UpdRepository) =
      IOHelpers.extractProtocolFromRequest
      >> Async.map (IOHelpers.extractUniqueCodesFromProtocol >> Seq.toList)

    let availableUpdates (db: UpdRepository) externalHost (escRepository: EscRepository) (compressedProtocol: byte[] option) userId =
      let getFromDb () =
        escRepository.Head userId
        |> Async.map (
          Seq.map (fst >> (fun eid -> (escRepository.GetDownloadLink eid externalHost, eid)))
          >> (fun efis -> let lst = Seq.toList efis in Ok (lst, lst |> ListAvailable |> AvailableMessage))
        )
      in
        compressedProtocol
        |> Option.map (
          getInstalledUpdatesFromProtocol db
          >> Async.bind (db.AddUpdatesByUniqueCode userId)
          >> Async.bind (ignore >> getFromDb)
        )
        |> Option.defaultValue (getFromDb ())

    // let convertToEsc (user: CustomerId) (db: UpdRepository) (cfg: Config) (escRepository: EscRepository) upds externalHost =
    //   // printfn "%A" host;
      
    //   if Set.isEmpty upds then
    //     Ok (None, ConvertToEscMessage EmptyUpdateList) |> Async.result
    //   else
    //     let f =
    //       upds
    //       |> Seq.map (db.GetUpdateUri externalHost)
    //       |> Seq.toList
    //       // |> Result.sequence
    //       // <?> (fun _ -> upds |> ConvertionSourceNotFound |> ConvertToEscMessage)
    //       // >>= (fun res ->
    //       //   let res =
    //       //     List.filter (function ({Name=MApteka},_) -> false | _ -> true) res

    //       //   let updSet =
    //       //     res |> List.map fst |> Set.ofList

    //       //   uploadEsc cfg user (List.map snd res)
    //       //   >>= (fun md5 ->
    //       //     escRepository.Put user md5 updSet false
    //       //     <!> (fun efi -> Some efi, (upds, efi) |> Converted |> ConvertToEscMessage)
    //       //     <?> UnexpectedError
    //       //   )
    //       // )
    //   ()
    let prepareToInstall (user: CustomerId) (db: UpdRepository) (upds: Update seq) =
      user
      |> List.replicate (Seq.length upds)
      |> Seq.zip upds
      |> Map.ofSeq
      |> db.AddToUsers

    let internal acceptDownloading (escRepository: EscRepository) (eid: EscId) user =
      user
      |> escRepository.Head
      |> Async.bind (
          Seq.filter (fst >> ((=) eid))
          >> Seq.collect (fun (_,(upds,_)) -> upds)
          >> Set.ofSeq
          >> (fun upds -> escRepository.Put user eid upds true)
      )
      |> Async.map (fun _ -> Ok ((), eid |> Ok |> AcceptDownloadingMessage))
  
  let internal interpretAsWebPart
    (cfg: Config)
    (srv: Services)
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
          |> keepGoingIfSucceedAsync next

      | AndThen (ResolveDependencies (upds, next)) ->
          resolveDependencies db upds
          |> keepGoingIfSucceedAsync next

      | AndThen (Publish ((upd, specs), next)) ->
          request (fun req ->
            req.files
            |> List.tryHead
            |> Option.toChoice (upd |> MissingFileBody |> PublishMessage)
            |> Result.ofChoice
            |> Result.map (fun file -> db.Upsert upd specs (IO.FileInfo file.tempFilePath))
            |> Result.mapAsync
            |> Async.map ^ Result.map (fun _ -> ((upd, specs), PublishMessage (Published upd)))
            |> keepGoingIfSucceedAsync next
          )

      | AndThen (GetAvailableUpdates (user, next)) ->
          let handle reqToBody =
            request (fun req ->
              let (Issuer customerId) = user
              in
                customerId
                |> availableUpdates db (req |> externalHost |> Uri) srv.EscRepository (reqToBody req)
                |> keepGoingIfSucceedAsync next
            )

          path "/api/v1/updates/available" >=> choose
            [ GET >=> handle (fun _ -> None)
              
              POST >=> handle (fun req -> Some req.rawForm)
            ]

      | AndThen (ConvertToEsc (user, upds, next)) ->
          request (fun req ->
            convertToEsc db srv.EscRepository cfg.EscConvertUri user upds (req |> externalHost |> Uri)
            |> keepGoingIfSucceedAsync next
          )
      
      | AndThen (PrepareToInstall (user, upds, next)) -> fun ctx ->
          prepareToInstall user db upds
          |> Async.bind (fun _ -> nextWebPart state next ctx)
          
      
      | AndThen (AcceptDownloading (user, uri, next)) ->
          acceptDownloading srv.EscRepository uri user
          |> keepGoingIfSucceedAsync (fun _ -> next)

    in // init state
      nextWebPart {Message=Never} program

  open Suave.Logging

  let interpretProgram (config: Config) (srv: Services) program =

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
          with  bindings = [ HttpBinding.create HTTP config.IP config.Port ]
                logger = logger
      }

    startWebServer serverConfig webpart
    
