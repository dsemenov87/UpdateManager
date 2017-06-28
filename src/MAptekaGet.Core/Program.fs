open System

[<EntryPoint>]
let main argv =
  // parse argv.[0] //"(=3.0.7 AND <3.0.8)"
  // // |> simplify
  // |> printfn "%A"
  
  // let r = And [Exactly (VersionInfo.parse "4.6.7"); Or [AtLeast (VersionInfo.parse "4.6.8"); Not (Exactly (VersionInfo.parse "4.6.7"))]]
  // let d = MAptekaRestriction.normalize r
  
  // let l1 = AtLeast (VersionInfo.parse "4.0.0")
  // let l2 = Exactly (VersionInfo.parse "4.5.0")
  // l1
  // |> normalize
  // |> removeNegatedLiteralsWhichOccurSinglePositive
  // |> removeSubsetLiteralsInAndClause
  // |> removeSubsetLiteralsInOrClause
  // |> removeUneccessaryAndClauses
  // |> removeUneccessaryOrClauses
  // |> replaceWithNoRestrictionIfAnyLiteralListIsEmpty
  // |> simplify
  // |> printfn "%A" 
  // l2 |> simplify |> printfn "%A" 
  // l1 .&& l2 |> printfn "%A" 
  0 // return an integer exit code
