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
  // let target =
  //   update <-- "rest-client-1.0.0" 
  //     <|*> (dependency <-- "http_client: 1.0.1 <= v <= 1.0.1" <!> addConstraint)
  //     <|*> (dependency <-- "json: 1.0.0 <= v <= 1.0.0" <!> addConstraint)
  
  // let lookupSet =
  //   [ target
      
  //     update <-- "http_client-1.0.1" 
  //       <|*> (dependency <-- "network: 1.2.2 <= v <= 1.2.2" <!> addConstraint)
        
  //     update <-- "network-1.2.2"
  //       <|*> (dependency <-- "bytes: 1.0.5 <= v < 2.0.0" <!> addConstraint)
        
  //     update <-- "network-2.0.1"

  //     update <-- "json-1.0.0" 
  //       <|*> (dependency <-- "bytes: 1.0.0 <= v <= 1.0.0" <!> addConstraint)

  //     update <-- "bytes-1.0.0"

  //     update <-- "bytes-1.0.7"
  //   ] 

  let ``rest-client-1.0.0`` : Update =
    { Name        = UpdateName "rest-client"
      Version     = {Major=1u;Minor=0u;Patch=0u}
      Constraints = [ Dependency (UpdateName "http_client", [ Range ( {Major=1u;Minor=0u;Patch=1u},
                                                                      RangeOp.LessOrEqual,
                                                                      RangeOp.LessOrEqual,
                                                                      {Major=1u;Minor=0u;Patch=1u}
                                                                    )
                                                            ])
                      Dependency (UpdateName "json", [ Range ( {Major=1u;Minor=0u;Patch=0u},
                                                                RangeOp.LessOrEqual,
                                                                RangeOp.LessOrEqual,
                                                                {Major=1u;Minor=0u;Patch=0u}
                                                              )
                                                      ])
                    ]
    }

  let ``http_client-1.0.1`` = 
    { Name        = UpdateName "http_client"
      Version     = {Major=1u;Minor=0u;Patch=1u}
      Constraints = [ Dependency (UpdateName "network", [ Range ( {Major=1u;Minor=2u;Patch=2u},
                                                                  RangeOp.LessOrEqual,
                                                                  RangeOp.LessOrEqual,
                                                                  {Major=1u;Minor=2u;Patch=2u}
                                                                )
                                                        ])]
    }

  let ``network-1.2.2`` = 
    { Name        = UpdateName "network"
      Version     = {Major=1u;Minor=2u;Patch=2u}
      Constraints = [ Dependency (UpdateName "bytes", [ Range ( {Major=1u;Minor=0u;Patch=5u},
                                                                RangeOp.LessOrEqual,
                                                                RangeOp.Less,
                                                                {Major=2u;Minor=0u;Patch=0u}
                                                              )
                                                      ])]
    }

  let ``network-2.0.1`` = 
    { Name        = UpdateName "network"
      Version     = {Major=2u;Minor=0u;Patch=1u}
      Constraints = []
    }

  let ``json-1.0.0`` = 
    { Name        = UpdateName "json"
      Version     = {Major=1u;Minor=0u;Patch=0u}
      Constraints = [ Dependency (UpdateName "bytes", [ Range ( {Major=1u;Minor=0u;Patch=0u},
                                                                RangeOp.LessOrEqual,
                                                                RangeOp.LessOrEqual,
                                                                {Major=1u;Minor=0u;Patch=0u}
                                                              )
                                                      ])]
    }

  let ``bytes-1.0.0`` = 
    { Name        = UpdateName "bytes"
      Version     = {Major=1u;Minor=0u;Patch=0u}
      Constraints = []
    }

  let ``bytes-1.0.7`` = 
    { Name        = UpdateName "bytes"
      Version     = {Major=1u;Minor=0u;Patch=7u}
      Constraints = []
    }
  
  let lookupSet = 
    [ ``rest-client-1.0.0``
      ``http_client-1.0.1``
      ``network-1.2.2``
      ``network-2.0.1``
      ``json-1.0.0``
      ``bytes-1.0.0``
      ``bytes-1.0.7``
    ]

  DR.candidateTree ``rest-client-1.0.0`` lookupSet
  |> Tree.map DR.labelInconsistent
  |> DR.pruneTree (Option.isSome << snd)
  |> DR.leaves
  |> NonEmptyList.map (fun ((s,_) as i) -> (i, DR.distance s))
  // |> (fun x -> printfn "%A" x ; x)
  |> NonEmptyList.sortBy snd
  // |> NonEmptyList.map (fun (((act, acts), conflict), dist) -> activationUpdate act, conflict, dist)
  |> DR.aggregateToResult lookupSet
  |> (PublishMessage << ResolutionMessage)
  |> Rep.printReport
  // |> Tree.map (fun x -> show (activationUpdate act) + " " + show label)
  // |> Rep.drawTree
  // |> println ^ Some 78
  |> printfn "%O"
  |> ignore

  // DR.candidateTree ``rest-client-1.0.0`` lookupSet
  // |> Tree.map DR.labelInconsistent
  // |> DR.pruneTree (Option.isSome << snd)
  // |> Tree.map (fun ((act,_),_) -> show (activationUpdate act))
  // |> Rep.drawTree
  // |> println ^ Some 78
  // |> ignore

  0 // return an integer exit code
