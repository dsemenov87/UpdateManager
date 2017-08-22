module DependencyResolutionSpecs
  
open Xunit
open FsUnit.Xunit
open MAptekaGet
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
  
// [<Fact>]
// let ``can check if constraints are incompatible`` () = async {
//   let ``rest_client-1.0.0`` =
//     upd "rest_client-1.0.0"
//     |> addConstraint ^ dep "http_client: 1.0.1 <= v <= 1.0.1"
//     |> addConstraint ^ dep "json: 1.0.0 <= v <= 1.0.0"

//   let lookupSet = 
//     [ ``rest_client-1.0.0``
//       upd "http_client-1.0.1" |> addConstraint ^ dep "network: 1.2.2 <= v <= 1.2.2"
//       upd "network-1.2.2"     |> addConstraint ^ dep "bytes: 1.0.5 <= v < 2.0.0"
//       upd "network-2.0.1"
//       upd "json-1.0.0"        |> addConstraint ^ dep "bytes: 1.0.0 <= v <= 1.0.0"
//       upd "bytes-1.0.0"
//       upd "bytes-1.0.7"
//     ]

//   let a1 =
//     ChildA (  upd "bytes-1.0.5",
//               dep "bytes: 1.0.5 <= v < 2.0.0",
//               ChildA (  upd "network-1.2.2",
//                         dep "network: 1.2.2 <= v <= 1.2.2",
//                         ChildA (  upd "http_client-1.0.1",
//                                   dep "http_client: 1.0.1 <= v <= 1.0.1",
//                                   InitialA ( ``rest_client-1.0.0`` ))))

//   let a2 =
//     ChildA (  upd "bytes-1.0.0",
//               dep "bytes: 1.0.0 <= v <= 1.0.0",
//               ChildA (  upd "json-1.0.0",
//                         dep "json: 1.0.0 <= v <= 1.0.0",
//                         InitialA ( ``rest_client-1.0.0`` )))

//   return!
//     DR.resolve (versionsOf lookupSet) (NEL.singleton ``rest_client-1.0.0``)
//     |> Async.map ^ shouldStrictEquals (IncompatibleConstraints (a2, a1))
// }

// [<Fact>]
// let ``if solution exists then returns Solution`` () = async {
//   let ``rest_client-0.9.9`` =
//     upd "rest_client-0.9.9"
//     |> addConstraint ^ dep "http_client: 1.0.0 <= v <= 1.0.0"
//     |> addConstraint ^ dep "json: 1.0.0 <= v <= 1.0.0"

//   let lookupSet = 
//     [ ``rest_client-0.9.9``
//       upd "http_client-1.0.1" |> addConstraint ^ dep "network: 1.2.2 <= v <= 1.2.2"
//       upd "http_client-1.0.0" |> addConstraint ^ dep "network: 1.0.0 <= v <= 1.0.0"
//       upd "network-1.2.2"     |> addConstraint ^ dep "bytes: 1.0.5 <= v < 2.0.0"
//       upd "network-1.0.0"     |> addConstraint ^ dep "bytes: 1.0.0 <= v <= 1.0.0"
//       upd "json-1.0.0"        |> addConstraint ^ dep "bytes: 1.0.0 <= v <= 1.0.0"
//       upd "bytes-1.0.0"
//       upd "bytes-1.0.7"
//     ]
//   return!
//     ``rest_client-0.9.9``
//     |> NEL.singleton
//     |> DR.resolve (versionsOf lookupSet)
//     |> Async.map (function
//       | Solution _  -> ()
//       | problem     -> failwith (string problem) 
//     )
// }

// [<Fact>]
// let ``can suggest clearest 4 names if missing update`` () = async
//   let lookupSet =
//     [
//       upd "http_client-1.0.1"
//       upd "network-1.2.2"
//       upd "common_nskPricingCheck-0.34.0"
//       upd "MApteka-2.27.0"
//       upd "MApteka-2.28.0"
//     ]
//     |> Set.ofList

//   upd "apt_ImportDocInt_221_nskPricingCheck-0.34.0"
//   |> addConstraint ^ dep "common_nskPricinkCheck: 0.34.0 <= v <= 0.34.0"
//   |> NEL.singleton
//   |> DR.resolve lookupSet
//   |> (function
//       | UpdateNotFound (  UpdateName "common_nskPricinkCheck", 
//                           ((UpdateName "common_nskPricingCheck" :: _) as sugg)) when sugg.Length = 4 -> ()
//       | others -> failwith (sprintf "%A" others) 
//     )