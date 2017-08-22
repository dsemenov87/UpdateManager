namespace MAptekaGet

module DataAccess =
  open System
  open System.Collections.Generic
  open System.Net.Http
  open Dist

  type UpdRepository =
    { HeadUpdate            : Update      -> Async<Choice<Option<Update * UpdateSpecs>, string>>
      GetUpdateUri          : Update      -> Uri -> Async<Option<Uri>>
      GetAvailableUpdates   : CustomerId  -> Async<Choice<Update Set, string>>
      AddUpdatesByUniqueCode: CustomerId  -> string list -> Async<Choice<unit, string>>
      GetVersionsByName     : UpdateName  -> Async<Update Set>
      // GetDependencies       : Update seq  -> Async<Update Set>
      Upsert                : Update      -> UpdateSpecs -> IO.FileInfo -> Async<Choice<unit, string>>
      AddToUsers            : Map<Update, CustomerId>
                                          -> Async<Choice<unit, string>>
      NearbyNames           : UpdateName  -> Async<Choice<UpdateName seq, string>>
    }

  type EscRepository =
    { Head  : CustomerId -> Async<Choice<(EscId * (Update Set * bool)) seq, string>>
      GetDownloadLink
            : EscId -> Uri -> Uri
      AcceptDownloading 
            : EscId -> Async<Choice<unit, string>>
      Create: CustomerId -> Update Set -> IO.Stream -> Async<Choice<EscId, string>>
      Delete: EscId -> Async<Choice<unit, string>>
    }

  let getEscUri (baseUri: Uri) (eid: EscId) =
    let ub = UriBuilder baseUri
    ub.Path <- ub.Path + "esc/" + eid.ToString("N").ToUpper() + ".esc"       
    ub.Uri

  let getUpdUri (baseUri: Uri) upd =
    let ub = UriBuilder baseUri
    ub.Path <- sprintf "upd/%O/%O" upd.Name upd.Version
    ub.Uri

  /// Use for developing/testing
  let inMemoryEscRepository (internalBaseUri: Uri) =
    
    let mutable escStorage : Map<CustomerId, (EscId * (Update Set * bool)) seq> = Map.empty
    
    let headEscByCustomerId cid =
      escStorage
      |> Map.tryFind cid
      |> Option.toList
      |> Seq.collect id
      |> Choice.create
      |> Async.result
  
    let acceptDownloading (eid: EscId) =
      async {
        escStorage <- 
          escStorage
          |> Map.map (fun _ seq' -> seq' |> Seq.map (fun ((eid', (upds,_)) as v) ->
              if eid = eid'
                then (eid', (upds, true))
                else v
          ));

        return Right ()
      }

    let createEscByCustomerId (cid: CustomerId) (upds: Update Set) (stream: IO.Stream) =
      async {      
        use uploader = new HttpClient()
        use uploadContent = new StreamContent(stream)
        
        let md5sum =
          (IO.calculateMd5 stream).ToUpper() |> Guid

        stream.Position <- 0L;

        let! response = uploader.PutAsync(getEscUri internalBaseUri md5sum, uploadContent) |> Async.AwaitTask
        response.EnsureSuccessStatusCode |> ignore
        
        let newValue =
          (md5sum, (upds, false))
        
        let newEntry =
          match Map.tryFind cid escStorage with
          | Some ecss -> Seq.append [newValue] ecss
          | None      -> seq [newValue]
        
        escStorage <- Map.add cid newEntry escStorage;
        
        return Right md5sum
      }

    let deleteEscByCustomerId eid =
      async {
        match escStorage
              |> Map.toSeq
              |> Seq.collect snd
              |> Seq.map fst
              |> Seq.tryFind ((=) eid) with
        | None -> return Right ()
            
        | Some eid' ->
            use hc = new Net.Http.HttpClient()
            let! response = hc.DeleteAsync (getEscUri internalBaseUri eid) |> Async.AwaitTask
            response.EnsureSuccessStatusCode |> ignore
            
            escStorage <-
              escStorage
              |> Map.map (fun _ seq' -> seq' |> Seq.filter (fst >> ((=) eid)));

            return Right ()
      }

    in {  Head = headEscByCustomerId
          GetDownloadLink = flip getEscUri
          AcceptDownloading = acceptDownloading
          Create = createEscByCustomerId
          Delete = deleteEscByCustomerId
       }

  /// Use for developing/testing
  let inMemoryUpdRepository (internalBaseUri: Uri) = 
    
    let ``mapteka-2.27`` =
      { Name        = MApteka
        Version     = {Major=2u; Minor=27u; Patch=0u}
        Constraints = []
      }

    let ``mapteka-2.28`` =
      { Name        = MApteka
        Version     = {Major=2u; Minor=28u; Patch=0u}
        Constraints = []
      }
    
    let mutable lookupSet : Map<Update, UpdateSpecs> = 
      [ (``mapteka-2.27``,
         (  { Author = ""
              Summary = ""
              UniqueCode = ""
              Description = ""
              ReleaseNotes = ""
              Created = DateTime.UtcNow
            }
         )
        )
        (``mapteka-2.28``,
         (  { Author = ""
              Summary = ""
              UniqueCode = ""
              Description = ""
              ReleaseNotes = ""
              Created = DateTime.UtcNow
            }
         )
        )
      ]
      |> Map.ofList

    let mutable installed : Map<Update, bool> =
      [(``mapteka-2.27``, false)]
      |> Map.ofList

    let getVersionsByName updName =
      lookupSet 
      |> Map.toList
      |> List.map fst
      |> List.filter (fun upd -> upd.Name = updName)
      |> Set.ofList

    { HeadUpdate = fun (upd: Update) ->
        lookupSet
        |> Map.tryFind upd
        // |> Result.ofOption (sprintf "Update '%O' not found." upd)
        |> Option.map (fun specs -> (upd, specs))
        |> Choice.create
        |> Async.result
      
      GetUpdateUri = fun upd externalHost ->
        lookupSet
        |> Map.tryFind upd
        |> Option.map (fun _ -> getUpdUri externalHost upd)
        |> Async.result

      GetVersionsByName = fun (updName: UpdateName) ->
        getVersionsByName updName |> Async.result

      // GetDependencies = fun (upds: Update seq) ->
      //   upds
      //   |> Seq.collect (fun upd ->
      //       upd.Constraints
      //       |> List.map (fun (Dependency (updName,_)) -> updName)
      //   )
      //   |> Seq.collect getVersionsByName
      //   |> Set.ofSeq
      //   |> Async.result
        
      Upsert = fun upd updspecs file ->
        async {
          use hc = new HttpClient()
          use fs = IO.File.OpenRead file.FullName :> IO.Stream
          use data =
            new StreamContent(fs)

          let uri =
            getUpdUri internalBaseUri upd

          let! response = hc.PutAsync (uri, data) |> Async.AwaitTask

          response.EnsureSuccessStatusCode() |> ignore

          lookupSet <- (Map.add upd updspecs) lookupSet
          
          return Right ()
        }

      AddToUsers = fun dict ->
        for (upd,_) in Map.toSeq dict do
          installed <- Map.add upd false installed;

        Async.result ^ Right ()

      GetAvailableUpdates = fun customerId ->
        installed
        |> Map.toList
        |> List.filter (not << snd) // non installed
        |> List.map fst
        |> Set.ofList
        |> Choice.create
        |> Async.result

      AddUpdatesByUniqueCode = fun customerId targetCodes ->
        targetCodes
        |> List.collect (fun targetCode ->
          lookupSet
          |> Map.filter (fun _ {UniqueCode=uniqueCode} -> uniqueCode = targetCode)
          |> Map.toList
          |> List.map fst
        )
        |> List.iter (fun upd ->
            if not (Map.containsKey upd installed) then
              installed <- Map.add upd true installed
        )
        |> Choice.create
        |> Async.result

      NearbyNames = fun (target: UpdateName) ->
        lookupSet
        |> Map.toList
        |> List.map (fun (upd,_) -> (target <--> upd.Name, upd.Name))
        |> List.sortBy fst
        |> List.map snd
        |> List.distinct
        |> Seq.truncate 4
        |> Choice.create
        |> Async.result
    }
