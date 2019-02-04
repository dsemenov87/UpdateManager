module DataAccess

open System
open System.Collections.Generic

open DomainServices
open Distance

// TODO complete sql
module Sql =
  open Npgsql
  open Dapper

  [<CLIMutable>]
  type UpdateInDb =
    { Id          : Guid
      Name        : string
      Major       : int
      Minor       : int
      Patch       : int
      Constraints : string
      Summary     : string
      Description : string
      Created     : DateTime
      ReleaseNotes: string
      Author      : string
      Uri         : string
    }

  let private executeSql<'a> connStr sql (params_: IDictionary<string, obj>) =
    async {
      use conn = new NpgsqlConnection(connStr)
      use cmd = conn.CreateCommand()

      try
        conn.OpenAsync() |> Async.AwaitTask |> ignore
        let! res =
          conn.QueryAsync<'a>(sql, params_) |> Async.AwaitTask

        return Right res
      with
        | :? NpgsqlException as ex ->
          return ex |> string |> Left
    }

  let checkIfUpdateExists connStr (upd: Update) =
      let sql =
        "SELECT 1 " +
        "FROM update " +
        "WHERE name = @name " +
        "AND major = @major " +
        "AND minor = @minor " +
        "AND patch = @patch;"

      let params_ =
        dict [  "name", upd.Name |> string |> box
                "major", upd.Version.Major |> box
                "minor", upd.Version.Minor |> box
                "patch", upd.Version.Patch |> box ]

      executeSql<int> connStr sql params_
      |> Async.map ^ Choice.map (Seq.isEmpty >> not)

[<AutoOpen>]
module InMemory =
  /// Use for developing/testing

  let ``default-platform`` =
    { Name        = Platform
      Version     = {Major=1u; Minor=0u; Patch=0u}
      Constraints = []
    }

  let mutable lookupSet : Map<Update, UpdateSpecs> = 
    [ (``default-platform``,
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
    [(``default-platform``, false)]
    |> Map.ofList

  let getVersionsByName updName =
    lookupSet 
    |> Map.toList
    |> List.map fst
    |> List.filter (fun upd -> upd.Name = updName)
    |> Set.ofList
    |> Async.result

  let checkIfUpdateExists (upd: Update) =
    lookupSet
    |> Map.tryFind upd
    |> Option.map (fun specs -> (upd, specs))
    |> Choice.create
    |> Async.result
      
  let inline getUpdUri (baseUri: Uri) upd =
      let ub = UriBuilder baseUri
      ub.Path <- sprintf "upd/%O/%O" upd.Name upd.Version
      ub.Uri

  let getUpdateUri = fun upd externalHost ->
    lookupSet
    |> Map.tryFind upd
    |> Option.map (fun _ -> getUpdUri externalHost upd)
    |> Async.result

      // GetDependencies = fun (upds: Update seq) ->
      //   upds
      //   |> Seq.collect (fun upd ->
      //       upd.Constraints
      //       |> List.map (fun (Dependency (updName,_)) -> updName)
      //   )
      //   |> Seq.collect getVersionsByName
      //   |> Set.ofSeq
      //   |> Async.result
        
  let upsert upd updspecs filePath =
    lookupSet <- (Map.add upd updspecs) lookupSet;
    Async.result (Right upd)

  let addToUsers dict =
    for (upd,_) in Map.toSeq dict do
      installed <- Map.add upd false installed;

    Async.result ^ Right ()

  let getAvailableUpdates customerId =
    installed
    |> Map.toList
    |> List.filter (not << snd) // non installed
    |> List.map fst
    |> Set.ofList
    |> Choice.create
    |> Async.result

  let addUpdatesByUniqueCode = fun customerId targetCodes ->
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

  let nearbyNames (target: UpdateName) =
    lookupSet
    |> Map.toList
    |> List.map (fun (upd,_) -> (target <--> upd.Name, upd.Name))
    |> List.sortBy fst
    |> List.map snd
    |> List.distinct
    |> Seq.truncate 4
    |> Choice.create
    |> Async.result
