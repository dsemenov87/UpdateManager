namespace MAptekaGet

module DataAccess =
  open System.Collections.Generic

  /// This is a data access wrapper around a any storage
  type IDbContext =
    abstract GetVersionsByName : UpdateName -> Result<Update list, string>
    
    abstract GetDependencies : Update list -> Result<Update list, string>
    
    abstract Upsert : UpdateInfo -> Result<Update, string>
    
  /// This class represents a in-memory storage
  type InMemoryDbContext() = 
    let mutable lookupSet : Map<Update, UpdateSpecs * UpdateFileInfo> = Map.empty
    let getVersionsByName updName =
      lookupSet 
      |> Map.toList
      |> List.map fst
      |> List.filter (fun upd -> upd.Name = updName)

    interface IDbContext with
      
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
        
      member this.Upsert ((upd, updspecs, fi): UpdateInfo) =
        lookupSet <- (Map.add upd (updspecs, fi) lookupSet)
        Ok upd