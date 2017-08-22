namespace MAptekaGet

module DataAccessSql =
  open System
  open System.Collections.Concurrent
  open FSharp.Control

  open Domain
  open DataAccess

  module DA = DataAccess

  open Sql
  open Sql.Operators
  open Npgsql
  open System.Data
  open Parsing

  module S = Select
  module I = Insert

  let safeToUint32 (x: int32) =
    x |> validateType<uint32> |> Option.defaultValue 0u

  type UpdateTbl() = static member Name () = "update"

  module UpdateCols =
    type UT = UpdateTbl
    
    let name    = txt<UT> "name"
    let major   = intgr<UT> "major"
    let minor   = intgr<UT> "minor"
    let patch   = intgr<UT> "patch"
    let created = dtime<UT> "created"
    let author  = txt<UT> "author"
    let summary = txt<UT> "summary"
    let descr   = txt<UT> "description"
    let constrs = txt<UT> "constraints"
    let uqCode  = txt<UT> "unique_code"

    let decodeVersion ((major, minor), patch) =
      { Major = major |> safeToUint32
        Minor = minor |> safeToUint32
        Patch = patch |> safeToUint32 }

    let decodeUpdate ((v, n), c: string) =
      (dependencies <-- c)
      |> Choice.bind (fun constrs ->
        updateName <-- n
        |> Choice.map (fun name -> {Name=name; Version=v; Constraints = constrs})
      )
      |> Choice.mapSnd Unknown

    let decodeUpdSpecs (((((upd, cr), a), s), d), uq) =
      let specs = { Author = a
                    Description = d
                    Summary = s
                    ReleaseNotes = ""
                    UniqueCode = uq
                    Created = cr }
      in
        Choice.bind (fun upd_ -> Right (upd_, specs)) upd

  module UT = UpdateCols

  type CustomerUpdateTbl() = static member Name () = "customer_update"

  module CustomerUpdateCols =
    type CUT = CustomerUpdateTbl

    let custId  = uuid<CUT> "customer_id"
    let name    = txt<CUT> "name"
    let major   = intgr<CUT> "major"
    let minor   = intgr<CUT> "minor"
    let patch   = intgr<CUT> "patch"
    let installed
                = booln<CUT> "installed"

  module CUT = CustomerUpdateCols

  type NearbyNamesVw() = static member Name () = "v_nearby_names"

  let sqlUpdRepository (connStr: string) (internalBaseUri: Uri) =
    let headUpdate (target: Update) =      
      async {
        use conn = new NpgsqlConnection(connStr)
        do! conn.OpenAsync()
        let! res =
          (UT.major |> S.where showInt (Eq, int target.Version.Major))
          .>>. (UT.minor |> S.where showInt (Eq, int target.Version.Minor))
          .>>. (UT.patch |> S.where showInt (Eq, int target.Version.Patch))
            |>> UT.decodeVersion 
          .>>. (UT.name  |> S.where showTxt (Eq, string target.Name))
          .>>. UT.constrs
            |>> UT.decodeUpdate
          .>>. UT.created .>>. UT.author .>>. UT.summary .>>. UT.descr .>>. UT.uqCode
            |>> UT.decodeUpdSpecs
          |> S.toRawSql
          |> S.toSeq conn
          |> Async.map ^ Seq.map ^ Choice.bind id
          |> Async.map (Seq.toList >> Choice.sequence >> Choice.map ^ Seq.tryHead >> Choice.mapSnd string)

        return res
      }

    let getUpdateUri upd externalHost =
      upd
      |> headUpdate
      |> Async.map (  Option.ofChoice
                  >>  Option.bind id
                  >>  Option.map (fst >> DA.getUpdUri externalHost))

    let getVersionsByName updName =
      async {
        use conn = new NpgsqlConnection(connStr)
        do! conn.OpenAsync()
        let! res =
            UT.major .>>. UT.minor .>>. UT.patch
              |>> UT.decodeVersion 
            .>>. (UT.name |> S.where showTxt (Eq, string updName))
            .>>. UT.constrs
              |>> UT.decodeUpdate
          |> S.toRawSql
          |> S.toSeq conn
          |> Async.map (Seq.map (Choice.bind id >> Choice.mapSnd string))
          |> Async.map ^ Seq.collect (function Left err -> failwith err | Right x -> [x]) // todo handle error
          |> Async.map ^ Set.ofSeq
        
        return res
      }

    let addUpdatesByUniqueCode (customerId: string) (targetCodes: list<_>) =
      let sqlExecStr =
        sprintf "UPDATE %s SET installed = TRUE WHERE customer_id='%s' AND name='%s' AND major=%d AND minor=%d AND patch=%d;"
           (CustomerUpdateTbl.Name())
           customerId
      
      let rec loop conn i =
        if i < targetCodes.Length then
          async {
            let! res =
                UT.name .>>. UT.major .>>. UT.minor .>>. UT.patch
                .>> (UT.uqCode |> S.where showTxt (Eq, List.item i targetCodes))
                  |>> (fun (((n, ma), mi), p) -> sqlExecStr n ma mi p)
              |> S.toRawSql
              |> S.toSeq conn
              |> Async.map (Seq.toList >> Choice.sequence >> Choice.map ^ Seq.tryHead >> Choice.mapSnd string)
              |> Async.map (Choice.bind (Option.map (exec conn) >> Choice.ofOption "doesn't exists."))
              |> Async.bind ^ Choice.revAsync 
              |> Async.map (Choice.mapSnd Unknown >> Choice.bind id)
            
            match res with
            | Left problem  -> return Left problem
            | Right _       -> return! loop conn (i + 1)
          }
        else
          Async.result ^ Right ()
        
      async {
        use conn = new NpgsqlConnection(connStr)
        do! conn.OpenAsync()

        let! res = loop conn 0
        return res |> Choice.mapSnd string
      }

    let nearbyNames target =
      async {
        use conn = new NpgsqlConnection(connStr)
        do! conn.OpenAsync()

        return!
          txt<NearbyNamesVw> "name"
          .>> txt<NearbyNamesVw> "of_name"
            |> S.where showTxt (Eq, string target)
          .>> (intgr<NearbyNamesVw> "dist" |> S.orderBy Asc)
          |> S.withLimit 4
          |> S.toRawSql
          |> S.toSeq conn
          |> Async.map (Seq.toList >> Choice.sequence >> Choice.map (Seq.map UpdateName) >> Choice.mapSnd string)
      }

    let upsert upd (specs: UpdateSpecs) (file: IO.FileInfo) =
      async {
        use hc = new Net.Http.HttpClient()
        use fs = IO.File.OpenRead file.FullName :> IO.Stream
        use data =
          new Net.Http.StreamContent(fs)

        let uri =
          getUpdUri internalBaseUri upd

        let! response = hc.PutAsync (uri, data) |> Async.AwaitTask

        response.EnsureSuccessStatusCode() |> ignore

        use conn = new NpgsqlConnection(connStr)
        do! conn.OpenAsync()

        let! res =
          Insert.into (UpdateTbl.Name())
            [ UT.name     |> I.pTxt (upd |> updName |> string)
              UT.major    |> I.pInt (int upd.Version.Major)
              UT.minor    |> I.pInt (int upd.Version.Minor)
              UT.patch    |> I.pInt (int upd.Version.Patch)
              UT.uqCode   |> I.pTxt specs.UniqueCode
              UT.author   |> I.pTxt specs.Author
              UT.summary  |> I.pTxt specs.Summary
              UT.descr    |> I.pTxt specs.Description
            ]
        |> exec conn
        
        return res |> Choice.mapSnd string |> Choice.map ignore
      }

    let addToUsers map' =
      let lst = Map.toList map'
      let rec loop conn i =
        if i < lst.Length then
          async {
            let (upd: Update, customerId: CustomerId) = List.item i lst
            let! res =  
              Insert.into (CustomerUpdateTbl.Name())
                [ CUT.custId  |> I.pUuid (Guid customerId)
                  UT.name     |> I.pTxt (upd |> updName |> string)
                  UT.major    |> I.pInt (int upd.Version.Major)
                  UT.minor    |> I.pInt (int upd.Version.Minor)
                  UT.patch    |> I.pInt (int upd.Version.Patch)
                ]
              |> exec conn

            match res with
            | Left err -> return Left err
            | Right res -> return! loop conn (i + 1) 
          }
        else
          Async.result ^ Right ()

      async {
        use conn = new NpgsqlConnection(connStr)
        do! conn.OpenAsync()

        let! res = loop conn 0
        return res |> Choice.mapSnd string
      }

    let getAvailableUpdates (customerId: CustomerId) =
      async {
        use conn = new NpgsqlConnection(connStr)
        do! conn.OpenAsync()

        let! res =
          CUT.name .>>. CUT.major .>>. CUT.minor .>>. CUT.patch
          .>> (CUT.custId |> S.where showUuid (Eq, Guid customerId))
          .>> (CUT.installed |> S.where showBln (Eq, false))
          |>> (fun (((n, ma), mi), p) ->
              (UT.major |> S.where showInt (Eq, ma))
              .>>. (UT.minor |> S.where showInt (Eq, int mi))
              .>>. (UT.patch |> S.where showInt (Eq, int p))
                |>> UT.decodeVersion 
              .>>. (UT.name  |> S.where showTxt (Eq, n))
              .>>. UT.constrs
                |>> UT.decodeUpdate
              .>>. UT.created .>>. UT.author .>>. UT.summary .>>. UT.descr .>>. UT.uqCode
                |>> UT.decodeUpdSpecs
              |> S.toRawSql
              |> S.toSeq conn
              |> Async.map ^ Seq.map ^ Choice.bind id
          )
          |> S.toRawSql
          |> S.toSeq conn
          |> Async.map ^ Seq.map ^ Choice.revAsync
          |> Async.bind ^ Async.Parallel
          |> Async.map (Seq.toList >> Choice.sequence)
          |> Async.map ^ Choice.map ^ Seq.collect id 
          |> Async.map ^ Choice.bind (Seq.toList >> Choice.sequence)
          
        return res |> Choice.map (List.map fst >> Set.ofList) |> Choice.mapSnd string
      }
    in
      { HeadUpdate = headUpdate
        GetUpdateUri = getUpdateUri
        GetAvailableUpdates = getAvailableUpdates
        AddUpdatesByUniqueCode = addUpdatesByUniqueCode
        GetVersionsByName = getVersionsByName
        Upsert = upsert
        AddToUsers = addToUsers
        NearbyNames = nearbyNames
      }




    
  type EscapeTbl() = static member Name () = "escape"

  [<RequireQualifiedAccess>]
  module EscapeTbl =
    type ET = EscapeTbl

    let escId   = uuid<ET> "esc_id"
    let fetched = booln<ET> "fetched"

  module ET = EscapeTbl

  type CustomerEscapeTbl() = static member Name () = "customer_escape"
  
  [<RequireQualifiedAccess>]
  module CustomerEscapeTbl =
    type CET = CustomerEscapeTbl 

    let customerId  = uuid<CET> "customer_id"
    let escId       = uuid<CET> "esc_id"
    let escUri      = txt<CET> "esc_uri"
    let constrs     = txt<CET> "constraints"
    let name        = txt<CET> "name"
    let major       = intgr<CET> "major"
    let minor       = intgr<CET> "minor"
    let patch       = intgr<CET> "patch"

  module CET = CustomerEscapeTbl
 
  let sqlEscRepository (connStr: string) (internalBaseUri: Uri) : EscRepository =
    let acceptDownloading (eid: EscId) =
      async {
        use conn = new NpgsqlConnection(connStr)
        do! conn.OpenAsync()
        
        return!
          sprintf "UPDATE customer_escape SET fetched = TRUE WHERE esc_id=%s" (showUuid eid)
          |> exec conn
          |> Async.map (Choice.map ignore >> Choice.mapSnd string) 
      }
    
    let createEscByCustomerId (cid: CustomerId) (upds: Update Set) (stream: IO.Stream) =
      async {      
        use uploader = new Net.Http.HttpClient()
        use uploadContent = new Net.Http.StreamContent(stream)
        
        let md5sum =
          stream |> IO.calculateMd5 |> Guid

        stream.Position <- 0L;

        let! response = uploader.PutAsync(getEscUri internalBaseUri md5sum, uploadContent) |> Async.AwaitTask
        response.EnsureSuccessStatusCode |> ignore
        
        use conn = new NpgsqlConnection(connStr)
        do! conn.OpenAsync()
        
        let! res =
          upds
          |> Seq.map (fun upd ->
            Insert.into (EscapeTbl.Name())
              [ ET.escId   |> I.pUuid md5sum
                ET.fetched |> I.pBln false
              ]
              |> exec conn
              |> Async.bind ^ Choice.bindAsync (fun (_:int) ->
                Insert.into (CustomerEscapeTbl.Name())
                  [ CET.customerId  |> I.pUuid (Guid cid)
                    CET.escId       |> I.pUuid md5sum
                    CET.name        |> I.pTxt (upd |> updName |> string)
                    CET.major       |> I.pInt (int upd.Version.Major)
                    CET.minor       |> I.pInt (int upd.Version.Minor) 
                    CET.patch       |> I.pInt (int upd.Version.Patch) 
                  ]
                  |> exec conn
              )
          )
          |> Async.Parallel
          |> Async.map (Array.toList >> Choice.sequence >> Choice.map ignore >> Choice.mapSnd string)
          
        return Choice.map (fun () -> md5sum) res
      }

    let deleteEsc (eid: EscId) =
      async {
        use conn = new NpgsqlConnection(connStr)
        do! conn.OpenAsync()
        
        return!
          sprintf "DELETE FROM escape WHERE esc_id=%s" (showUuid eid)
          |> exec conn
          |> Async.bind ^ Choice.mapAsync (fun (count: int) -> 
              async {
                if count > 0 then
                  use hc = new Net.Http.HttpClient()
                  let! response = hc.DeleteAsync (getEscUri internalBaseUri eid) |> Async.AwaitTask
                  response.EnsureSuccessStatusCode |> ignore
                  return ()
              }
          )
          |> Async.map ^ Choice.mapSnd string
      }

    let headEscByCustomerId (cid: CustomerId) =
      async {
        use conn = new NpgsqlConnection(connStr)
        do! conn.OpenAsync()

        let! eitherCustomerUpds =
          CET.name .>>. CET.major .>>. CET.minor .>>. CET.patch .>>. CET.escId
          .>> (CET.customerId |> S.where showUuid (Eq, Guid cid))
          |> S.toRawSql
          |> S.toSeq conn

        let! eitherCustomerUpds =
          eitherCustomerUpds
          |> Seq.map ^ Choice.bindAsync (fun ((((n, ma), mi), p), eid) ->
              UT.constrs
              .>> (UT.name  |> S.where showTxt (Eq, n))
              .>> (UT.major |> S.where showInt (Eq, ma))
              .>> (UT.minor |> S.where showInt (Eq, mi))
              .>> (UT.patch |> S.where showInt (Eq, p))
              |>> (fun c -> UT.decodeUpdate ((UT.decodeVersion ((ma, mi), p), n), c))
              |> S.toRawSql
              |> S.toSeq conn
              |> Async.map (
                Seq.map ^ Choice.bind id
                >> Seq.toList >> Choice.sequence
                >> Choice.map (fun upds -> (eid, Set.ofList upds)))
          )
          |> Async.Parallel

        let! res =
          eitherCustomerUpds
          |> Array.map ^ Choice.bindAsync (fun (eid, upds) ->
              ET.fetched .>> (ET.escId |> S.where showUuid (Eq, eid))
              |> S.toRawSql
              |> S.toSeq conn
              |> Async.map (
                Seq.tryHead
                >> Option.toChoice (Unknown (sprintf "Database inconsistency: Esc '%O' doesn't exist." eid))
                >> Choice.bind id
                >> Choice.map (fun fetched -> (eid, (upds, fetched)))
              )
          )
          |> Async.Parallel
          |> Async.map (Array.toList >> Choice.sequence >> Choice.map List.toSeq)

        return Choice.mapSnd string res
      }
    in
      { Head              = headEscByCustomerId
        GetDownloadLink   = flip getEscUri
        AcceptDownloading = acceptDownloading
        Create            = createEscByCustomerId
        Delete            = deleteEsc
      }


    
    