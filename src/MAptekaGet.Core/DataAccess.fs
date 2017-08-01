namespace MAptekaGet

module DataAccess =
  open System
  open System.Collections.Generic
  open System.Net.Http
  open ResultOp

  /// This is a data access wrapper around a any storage
  type IDbContext =
    abstract GetUpdate : Update -> Result<UpdateInfo, string>
    
    abstract GetUpdateUri : Update -> Result<Update * Uri, string>

    abstract GetAvailableUpdates : CustomerId -> Result<Update Set, string>

    abstract AddUpdatesByUniqueCode : string list * CustomerId -> Async<Result<unit, string>>
    
    abstract GetVersionsByName : UpdateName -> Result<Update Set, string>
    
    abstract GetDependencies : Update seq -> Result<Update Set, string>
    
    abstract Upsert : Update * UpdateSpecs * IO.Stream -> Result<Update, string>

    abstract AddToUsers : Map<Update, CustomerId> -> Result<Map<Update, CustomerId>, string>

    abstract AcceptDownloading : Update * CustomerId -> Result<Update, string>

    abstract AcceptInstallation : Update * CustomerId -> Result<Update, string>

  type EscRepository =
    { Get: Update -> Result<EscFileInfo option, string>
      Put: Update -> EscFileInfo -> Result<Update * EscFileInfo, string>
    }

  /// This class represents an in-memory storage
  type InMemoryDbContext(baseUri) = 
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

    let mutable installed : Map<Update, bool * bool> =
      [``mapteka-2.27``, (true, true)]
      |> Map.ofList

    let getVersionsByName updName =
      lookupSet 
      |> Map.toList
      |> List.map fst
      |> List.filter (fun upd -> upd.Name = updName)
      |> Set.ofList

    interface IDbContext with
      member this.GetUpdate (upd: Update) =
        lookupSet
        |> Map.tryFind upd
        |> Result.ofOption (sprintf "Update '%O' not found." upd)
        <!> (fun specs -> upd, specs)
      
      member this.GetUpdateUri upd = // todo look at lookup Set!!
        (upd, sprintf "%s/%O/%O" baseUri upd.Name upd.Version |> Uri)
        |> Ok

      member this.GetVersionsByName (updName: UpdateName) =
        getVersionsByName updName |> Ok

      member this.GetDependencies (upds: Update seq)  =
        upds
        |> Seq.collect (fun upd ->
            upd.Constraints
            |> List.map (fun (Dependency (updName,_)) -> updName)
        )
        |> Seq.collect getVersionsByName
        |> Set.ofSeq
        |> Ok
        
      member this.Upsert (upd, updspecs, stream) =
        async {
          use hc = new HttpClient()
          use ms = new IO.MemoryStream()
          stream.CopyTo ms
          
          let uri =
            sprintf "%s/%O/%O" baseUri upd.Name upd.Version

          use data =
            new StreamContent(stream)

          lookupSet <- (Map.add upd updspecs) lookupSet
          return upd
        }
        |> Async.Catch
        |> Async.RunSynchronously
        |> Result.ofChoice
        <?> string

      member this.AddToUsers (dict) =
        for (upd,_) in Map.toSeq dict do
          installed <- Map.add upd (false, false) installed;

        Ok dict

      member this.AcceptDownloading (upd, pharmacyId) =
        installed <- Map.add upd (true, false) installed
        Ok upd

      member this.AcceptInstallation (upd, pharmacyId) =
        installed <- Map.add upd (true, true) installed
        Ok upd

      member this.GetAvailableUpdates (customerId) =
        installed
        |> Map.toList
        // |> List.filter (fun (_,(i,d)) -> i && d)
        |> List.map fst
        |> Set.ofList
        |> Ok

      member this.AddUpdatesByUniqueCode (targetCodes, customerId) =
        targetCodes
        |> List.collect (fun targetCode ->
          lookupSet
          |> Map.filter (fun _ {UniqueCode=uniqueCode} -> uniqueCode = targetCode)
          |> Map.toList
          |> List.map fst
        )
        |> List.iter (fun upd ->
            if not (Map.containsKey upd installed) then
              installed <- Map.add upd (false, false) installed
        )
        |> Ok
        |> Async.result