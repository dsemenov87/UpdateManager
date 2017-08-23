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

    let custId  = txt<CUT> "customer_id"
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
      |> S.toSeq connStr
      |> Async.map ^ Seq.map ^ Choice.bind id
      |> Async.map (Seq.toList >> Choice.sequence >> Choice.map ^ Seq.tryHead >> Choice.mapSnd string)

    let getUpdateUri upd externalHost =
      upd
      |> headUpdate
      |> Async.map (  Option.ofChoice
                  >>  Option.bind id
                  >>  Option.map (fst >> DA.getUpdUri externalHost))

    let getVersionsByName updName =
      UT.major .>>. UT.minor .>>. UT.patch
        |>> UT.decodeVersion 
      .>>. (UT.name |> S.where showTxt (Eq, string updName))
      .>>. UT.constrs
        |>> UT.decodeUpdate
      |> S.toRawSql
      |> S.toSeq connStr
      |> Async.map (Seq.map (Choice.bind id >> Choice.mapSnd string))
      |> Async.map ^ Seq.collect (function Left err -> failwith err | Right x -> [x]) // todo handle error
      |> Async.map ^ Set.ofSeq

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
        
      in
        loop connStr 0 |> Async.map ^ Choice.mapSnd string

    let nearbyNames target =
      txt<NearbyNamesVw> "name"
      .>> txt<NearbyNamesVw> "of_name"
        |> S.where showTxt (Eq, string target)
      .>> (intgr<NearbyNamesVw> "dist" |> S.orderBy Asc)
      |> S.withLimit 4
      |> S.toRawSql
      |> S.toSeq connStr
      |> Async.map (Seq.toList >> Choice.sequence >> Choice.map (Seq.map UpdateName) >> Choice.mapSnd string)

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
        |> exec connStr
        
        return res |> Choice.mapSnd string |> Choice.map ignore
      }

    let addToUsers map' =
      let lst = Map.toList map'
      let rec loop connStr i =
        if i < lst.Length then
          async {
            let (upd: Update, customerId: CustomerId) = List.item i lst
            let! res =  
              (CUT.custId     |> S.where showTxt (Eq, customerId))
              .>> (CUT.name   |> S.where showTxt (Eq, string upd.Name))
              .>> (CUT.major  |> S.where showInt (Eq, int upd.Version.Major))
              .>> (CUT.minor  |> S.where showInt (Eq, int upd.Version.Minor))
              .>> (CUT.patch  |> S.where showInt (Eq, int upd.Version.Patch))
              |> S.toRawSql
              |> S.toSeq connStr
              |> Async.bind (
                Seq.toList >> Choice.sequence >> Choice.bindAsync (function
                  | [] ->
                    Insert.into (CustomerUpdateTbl.Name())
                      [ CUT.custId  |> I.pTxt customerId
                        UT.name     |> I.pTxt (upd |> updName |> string)
                        UT.major    |> I.pInt (int upd.Version.Major)
                        UT.minor    |> I.pInt (int upd.Version.Minor)
                        UT.patch    |> I.pInt (int upd.Version.Patch)
                      ]
                    |> exec connStr

                  | _ -> Async.result ^ Right 0
                )
              )

            match res with
            | Left err -> return Left err
            | Right _ -> return! loop connStr (i + 1) 
          }
        else
          Async.result ^ Right ()

      in
        loop connStr 0 |> Async.map ^ Choice.mapSnd string

    let getAvailableUpdates (customerId: CustomerId) =
      CUT.name .>>. CUT.major .>>. CUT.minor .>>. CUT.patch
      .>> (CUT.custId |> S.where showTxt (Eq, customerId))
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
          |> S.toSeq connStr
          |> Async.map ^ Seq.map ^ Choice.bind id
      )
      |> S.toRawSql
      |> S.toSeq connStr
      |> Async.map ^ Seq.map ^ Choice.revAsync
      |> Async.bind ^ Async.Parallel
      |> Async.map (Seq.toList >> Choice.sequence)
      |> Async.map ^ Choice.map ^ Seq.collect id 
      |> Async.map ^ Choice.bind (Seq.toList >> Choice.sequence)
      |> Async.map (Choice.map (List.map fst >> Set.ofList) >> Choice.mapSnd string)
      
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

  type EscapeUpdateTbl() = static member Name () = "escape_update"
  
  [<RequireQualifiedAccess>]
  module EscapeUpdateTbl =
    type EUT = EscapeUpdateTbl 

    let customerId  = txt<EUT> "customer_id"
    let escId       = uuid<EUT> "esc_id"
    let escUri      = txt<EUT> "esc_uri"
    let constrs     = txt<EUT> "constraints"
    let name        = txt<EUT> "name"
    let major       = intgr<EUT> "major"
    let minor       = intgr<EUT> "minor"
    let patch       = intgr<EUT> "patch"

  module EUT = EscapeUpdateTbl 
 
  let sqlEscRepository (connStr: string) (internalBaseUri: Uri) : EscRepository =
    let acceptDownloading (eid: EscId) =
      sprintf "UPDATE escape SET fetched = TRUE WHERE esc_id=%s" (showUuid eid)
      |> exec connStr
      |> Async.map (Choice.map ignore >> Choice.mapSnd string)
    
    let createEscByCustomerId (cid: CustomerId) (upds: Update Set) (stream: IO.Stream) =
      async {      
        use uploader = new Net.Http.HttpClient()
        use uploadContent = new Net.Http.StreamContent(stream)
        
        let md5sum =
          stream |> IO.calculateMd5 |> Guid

        stream.Position <- 0L;

        let! response = uploader.PutAsync(getEscUri internalBaseUri md5sum, uploadContent) |> Async.AwaitTask
        response.EnsureSuccessStatusCode |> ignore
        
        let! res =
          ET.escId
          |> S.where showUuid (Eq, md5sum)
          |> S.toRawSql |> S.toSeq connStr
          |> Async.bind (
            Seq.toList >> Choice.sequence >> Choice.bindAsync (function
              | [] ->
                  Insert.into (EscapeTbl.Name())
                    [ ET.escId   |> I.pUuid md5sum
                      ET.fetched |> I.pBln false
                    ]
                    |> exec connStr
                    |> Async.bind ^ Choice.bindAsync (fun (_:int) ->
                      upds
                      |> Seq.map (fun upd ->
                        ET.escId
                        |> S.where showUuid (Eq, md5sum)
                        |> S.toRawSql |> S.toSeq connStr
                        |> Async.bind (
                          Seq.toList >> Choice.sequence >> Choice.bindAsync (function
                            | [] ->
                                Insert.into (EscapeTbl.Name())
                                  [ ET.escId   |> I.pUuid md5sum
                                    ET.fetched |> I.pBln false
                                  ]
                                  |> exec connStr
                                  |> Async.bind ^ Choice.bindAsync (fun (_:int) ->
                                    Insert.into (EscapeUpdateTbl.Name())
                                      [ EUT.customerId  |> I.pTxt cid
                                        EUT.escId       |> I.pUuid md5sum
                                        EUT.name        |> I.pTxt (upd |> updName |> string)
                                        EUT.major       |> I.pInt (int upd.Version.Major)
                                        EUT.minor       |> I.pInt (int upd.Version.Minor) 
                                        EUT.patch       |> I.pInt (int upd.Version.Patch) 
                                      ]
                                      |> exec connStr
                                  )
                            | _ ->
                                Async.result ^ Right 0
                          ))
                      )
                      |> Async.Parallel
                      |> Async.map (Array.toList >> Choice.sequence >> Choice.map ignore)
                    )
              | _ ->
                  Async.result ^ Right ()
          ))
          |> Async.map ^ Choice.mapSnd string

        return Choice.map (fun () -> md5sum) res
      }

    let deleteEsc (eid: EscId) =
      sprintf "DELETE FROM escape WHERE esc_id=%s" (showUuid eid)
      |> exec connStr
      |> Async.map ^ Choice.mapSnd string
      |> Async.bind ^ Choice.bindAsync (fun (count: int) -> 
          async {
            if count > 0 then
              use hc = new Net.Http.HttpClient()
              let! response = hc.DeleteAsync (getEscUri internalBaseUri eid) |> Async.AwaitTask
              return if response.IsSuccessStatusCode
                        then Right ()
                        else Left ^ sprintf "cannot delete '%O'.esc file. HTTP status code: %O." eid response.StatusCode 
            else
              return Right ()
          }
      )

    let headEscByCustomerId (cid: CustomerId) =
      async {
        let! eitherCustomerUpds =
          EUT.name .>>. EUT.major .>>. EUT.minor .>>. EUT.patch .>>. EUT.escId
          .>> (EUT.customerId |> S.where showTxt (Eq, cid))
          |> S.toRawSql
          |> S.toSeq connStr

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
              |> S.toSeq connStr
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
              |> S.toSeq connStr
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


    
    
