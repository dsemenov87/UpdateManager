module DependencyResolutionSpecs
  
open Xunit
open FsUnit.Xunit
open CommonUtils

module DR = DependencyResolution

let versionsOf lookup updName =
  lookup
  |> List.filter (fun v -> v.Name = updName)
  |> List.sortBy (fun v -> v.Version)
  |> List.rev // newest is always better
  |> Set.ofList
  |> Async.result

[<Fact>]
let ``can check if dependency version is missing`` () = async {
  let ``http_client = 1.0.5`` =
    dep "http_client: 1.0.5 <= v <= 1.0.5"

  let ``ws_client-1.0.0`` =
    upd "ws_client-1.0.0" |> addConstraint ``http_client = 1.0.5`` 

  let childA x =
    ChildA (x, ``http_client = 1.0.5``, InitialA ``ws_client-1.0.0``)
  
  let lookupSet =
    [ ``ws_client-1.0.0``
      upd "http_client-1.0.0" 
      upd "http_client-1.0.2"
      upd "http_client-1.0.3"
    ]
 
  return!
    ``ws_client-1.0.0``
    |> NEL.singleton
    |> DR.resolve (versionsOf lookupSet)
    |> Async.map ^ shouldStrictEquals ^ MissingUpdateVersion (ChildA (upd "http_client-1.0.0", ``http_client = 1.0.5``, InitialA ``ws_client-1.0.0``))
}
  