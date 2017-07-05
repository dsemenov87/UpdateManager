open System

open MAptekaGet
open MAptekaGet.Reporting
open MAptekaGet.Utils.Parsing
open MAptekaGet.Utils.ResultOp
open MAptekaGet.Domain.Parsing
open FParsec

module DR = DependencyResolution
module Rep = Reporting

[<EntryPoint>]
let main argv =
  let upd txt =
   match update <-- txt with Result.Ok x -> x | _ -> invalidArg "txt" "schould be valid update name."

  let dep txt =
     match dependency <-- txt with Result.Ok x -> x | _ -> invalidArg "txt" "schould be valid dependency name."
  
  let ``rest_client-0.9.9`` =
    upd "rest_client-0.9.9"
    |> addConstraint ^ dep "http_client: 1.0.0 <= v <= 1.0.0"
    |> addConstraint ^ dep "json: 1.0.0 <= v <= 1.0.0"

  let lookupSet = 
    [ ``rest_client-0.9.9``
      upd "http_client-1.0.1" |> addConstraint ^ dep "network: 1.2.2 <= v <= 1.2.2"
      upd "http_client-1.0.0" |> addConstraint ^ dep "network: 1.0.0 <= v <= 1.0.0"
      upd "network-1.2.2"     |> addConstraint ^ dep "bytes: 1.0.5 <= v < 2.0.0"
      upd "network-1.0.0"     |> addConstraint ^ dep "bytes: 1.0.0 <= v <= 1.0.0"
      upd "json-1.0.0"        |> addConstraint ^ dep "bytes: 1.0.0 <= v <= 1.0.0"
      upd "bytes-1.0.0"
      upd "bytes-1.0.7"
    ]


  // DR.candidateTree lookupSet (NonEmptyList.singleton ``rest_client-0.9.9``)
  // |> Tree.map DR.labelInconsistent
  // |> DR.pruneTree (Option.isSome << snd)
  // |> DR.leaves
  // |> NonEmptyList.map (fun ((s,_) as i) -> (i, DR.distance s))
  // // |> (fun x -> printfn "%A" x ; x)
  // |> NonEmptyList.sortBy snd
  // // |> NonEmptyList.map (fun (((act, acts), conflict), dist) -> activationUpdate act, conflict, dist)
  // |> DR.aggregateToResult lookupSet
  // |> (CheckMessage << ResolutionMessage)
  // |> Rep.printReport
  // // |> Tree.map (fun x -> show (activationUpdate act) + " " + show label)
  // // |> Rep.drawTree
  // // |> println ^ Some 78
  // |> printfn "%O"
  // |> ignore

  // DR.candidateTree ``rest_client-1.0.0`` lookupSet
  // |> Tree.map DR.labelInconsistent
  // |> DR.pruneTree (Option.isSome << snd)
  // |> Tree.map (fun ((act,_),_) -> show (activationUpdate act))
  // |> Rep.drawTree
  // |> println ^ Some 78
  // |> ignore

  0 // return an integer exit code
