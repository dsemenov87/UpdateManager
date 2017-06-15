// Learn more about F# at http://fsharp.org

open System
open MAptekaGet.Domain
open MAptekaGet.Domain.MAptekaRestriction

[<EntryPoint>]
let main argv =
  // let r = And [Exactly (VersionInfo.parse "4.6.7"); Or [AtLeast (VersionInfo.parse "4.6.8"); Not (Exactly (VersionInfo.parse "4.6.7"))]]
  // let d = MAptekaRestriction.normalize r
  
  let l1 = AtLeast (VersionInfo.parse "4.0.0")
  let l2 = Exactly (VersionInfo.parse "4.5.0")
  l1
  |> normalize
  |> removeNegatedLiteralsWhichOccurSinglePositive
  |> removeUneccessaryOrClauses
  |> printfn "%A" 
  // l2 |> simplify |> printfn "%A" 
  // l1 .&& l2 |> printfn "%A" 
  0 // return an integer exit code
