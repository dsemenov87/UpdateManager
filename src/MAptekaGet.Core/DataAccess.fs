namespace MAptekaGet

module DataAccess =
  open System
  open System.Collections.Generic
  open System.Net.Http
  open ResultOp

  let [<Literal>] BaseUri =
    "http://test-mapteka-updater.itapteka.loc/upd/"

  /// This is a data access wrapper around a any storage
  type IDbContext =
    abstract GetUpdate : Update -> Result<Update * UpdateSpecs, string>
    
    abstract GetUpdateUri : Update list -> Result<(Update * Uri) list, string>

    abstract GetAvailableUpdates : CustomerId -> Result<Update list, string>
    
    abstract GetVersionsByName : UpdateName -> Result<Update list, string>
    
    abstract GetDependencies : Update list -> Result<Update list, string>
    
    abstract Upsert : UpdateInfo * IO.Stream -> Result<Update, string>

    abstract AddToUsers : Map<Update, CustomerId> -> Result<Map<Update, CustomerId>, string>

    abstract AcceptDownloading : Update * CustomerId -> Result<Update, string>

    abstract AcceptInstallation : Update * CustomerId -> Result<Update, string>

  /// This class represents an in-memory storage
  type InMemoryDbContext() = 
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
              Description = ""
              ReleaseNotes = ""
              Created = DateTime.UtcNow
            }
         )
        )
        (``mapteka-2.28``,
         (  { Author = ""
              Summary = ""
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

    interface IDbContext with
      member this.GetUpdate (upd: Update) =
        lookupSet
        |> Map.tryFind upd
        |> ResultExt.fromOption (sprintf "Update '%O' not found." upd)
        <!> (fun specs -> upd, specs)
      
      member this.GetUpdateUri upds = // todo look at lookup Set!!
        upds
        |> List.map (fun upd -> (upd, sprintf "%s/%O/%O" BaseUri upd.Name upd.Version |> Uri))
        |> Ok

      member this.GetVersionsByName (updName: UpdateName) =
        getVersionsByName updName |> Ok

      member this.GetDependencies (upds: Update list)  =
        upds
        |> List.collect (fun upd ->
            upd.Constraints
            |> List.map (fun (Dependency (updName,_)) -> updName)
        )
        |> List.collect getVersionsByName
        |> Ok
        
      member this.Upsert ((upd, updspecs), stream) =
        async {
          use hc = new HttpClient()
          use ms = new IO.MemoryStream()
          stream.CopyTo ms
          
          let uri =
            sprintf "%s/%O/%O" BaseUri upd.Name upd.Version

          use data =
            new StreamContent(stream)

          // let! resp = hc.PutAsync(uri, data) |> Async.AwaitTask

          lookupSet <- (Map.add upd updspecs) lookupSet
          return upd
        }
        |> Async.Catch
        |> Async.RunSynchronously
        |> ResultExt.fromChoice
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
        |> Ok