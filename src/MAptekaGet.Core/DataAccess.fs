namespace MAptekaGet

module DataAccess =
  open System
  open System.Collections.Generic
  open System.Net.Http
  open ResultOp

  type UpdRepository =
    { HeadUpdate            : Update      -> Async<Option<Update * UpdateSpecs>>
      GetUpdateUri          : Update      -> Uri -> Async<Option<Uri>>
      GetAvailableUpdates   : CustomerId  -> Async<Update Set>
      AddUpdatesByUniqueCode: CustomerId  -> string list -> Async<unit>
      GetVersionsByName     : UpdateName  -> Async<Update Set>
      GetDependencies       : Update seq  -> Async<Update Set>
      Upsert                : Update      -> UpdateSpecs -> IO.FileInfo -> Async<Update>
      AddToUsers            : Map<Update, CustomerId>
                                          -> Async<Map<Update, CustomerId>>
    }

  type EscRepository =
    { Head  : CustomerId -> Async<(EscId * (Update Set * bool)) seq>
      GetDownloadLink
            : EscId -> Uri -> Uri
      Put   : CustomerId -> EscId -> Update Set -> bool -> Async<EscId>
      Create: CustomerId -> Update Set -> IO.Stream -> Async<EscId>
      Delete: CustomerId -> EscId -> Async<unit>
    }

  let inMemoryEscRepository (internalBaseUri: Uri) =
    
    let mutable escStorage : Map<CustomerId, (EscId * (Update Set * bool)) seq> = Map.empty
    
    let getEscUri (baseUri: Uri) (eid: EscId) =
      let ub = UriBuilder baseUri
      ub.Path <- ub.Path + "esc/" + eid.ToString("N").ToUpper() + ".esc"

      printfn "%A" ub.Uri        

      ub.Uri
    
    let headEscByCustomerId cid =
      escStorage
      |> Map.tryFind cid
      |> Option.toList
      |> Seq.collect id
      |> Async.result
  
    let putEscByCustomerId (cid: CustomerId) (eid: EscId) (upds: Update Set) (fetched: bool) =
      async {
        let newValue =
          let entry = [(eid, (upds, fetched))]
          match Map.tryFind cid escStorage with
          | None ->
            seq entry
          | Some ecss ->
            ecss
            |> Seq.filter (fst >> ((<>) eid))
            |> Seq.append entry
                  
        escStorage <- Map.add cid newValue escStorage;
        
        return eid
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
        
        return md5sum
      }

    let deleteEscByCustomerId cid eid =
      async {
        match Map.tryFind cid escStorage with
        | None -> ()
            
        | Some ecss ->
            let newValue =
              Seq.filter (fst >> ((<>) eid)) ecss
            
            use hc = new HttpClient()
            let! response = hc.DeleteAsync (getEscUri internalBaseUri eid) |> Async.AwaitTask
            response.EnsureSuccessStatusCode |> ignore
            
            escStorage <- Map.add cid newValue escStorage;
      }

    in {  Head = headEscByCustomerId
          GetDownloadLink = flip getEscUri
          Put = putEscByCustomerId
          Create = createEscByCustomerId
          Delete = deleteEscByCustomerId
       }

  
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
      [(``mapteka-2.27``, true)]
      |> Map.ofList

    let getUpdUri (baseUri: Uri) upd =
      let ub = UriBuilder baseUri
      ub.Path <- sprintf "upd/%O/%O" upd.Name upd.Version
      ub.Uri

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
        |> Async.result
      
      GetUpdateUri = fun upd externalHost ->
        lookupSet
        |> Map.tryFind upd
        |> Option.map (fun _ -> getUpdUri externalHost upd)
        |> Async.result

      GetVersionsByName = fun (updName: UpdateName) ->
        getVersionsByName updName |> Async.result

      GetDependencies = fun (upds: Update seq) ->
        upds
        |> Seq.collect (fun upd ->
            upd.Constraints
            |> List.map (fun (Dependency (updName,_)) -> updName)
        )
        |> Seq.collect getVersionsByName
        |> Set.ofSeq
        |> Async.result
        
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
          
          return upd
        }

      AddToUsers = fun dict ->
        for (upd,_) in Map.toSeq dict do
          installed <- Map.add upd false installed;

        async { return dict }

      GetAvailableUpdates = fun customerId ->
        installed
        |> Map.toList
        |> List.filter snd // non fetched
        |> List.map fst
        |> Set.ofList
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
        |> Async.result
    }
