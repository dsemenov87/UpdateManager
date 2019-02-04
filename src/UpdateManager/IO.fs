module internal IOHelpers

open System.Xml.Linq

let TempDir = env "ESC_TEMP_DIR" |> Choice.orDefault "__temp__"

let extractUniqueCodesFromProtocol (input: IO.Stream) =
  Text.Encoding.RegisterProvider(Text.CodePagesEncodingProvider.Instance); // we need win-1251 ...
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
      match req.header "X-User" with
      | Choice1Of2 user ->
          nextWebPart state (next (Issuer user))

      | _ ->
          UNAUTHORIZED "You should set 'X-User' header." 
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
    |> Async.bind (function
      | Right _   -> nextWebPart state next ctx
      | Left err  -> failwith err
    )
    

| AndThen (AcceptDownloading (user, uri, next)) ->
    acceptDownloading srv.EscRepository uri user
    |> keepGoingIfSucceedAsync (fun _ -> next)

in // init state
nextWebPart {Message=Never} program

let interpretProgram (config: Config) (db: UpdRepository) (srv: Services) program =    


