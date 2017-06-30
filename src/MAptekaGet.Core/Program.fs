open System

open MAptekaGet
open MAptekaGet.Reporting
open MAptekaGet.Utils.Parsing
open MAptekaGet.Utils.ResultOperators
open MAptekaGet.Domain.Parsing
open FParsec

[<EntryPoint>]
let main argv =
  
  let tree =
    Node
      ( "ws-client-1.0.0" ,
        [ Node
            ( "http-client-1.0.5",
              [ Node
                  ( "network-2.1.3",
                    [ Node
                        ( "xml-parser-1.2.0",
                          [ Node ("det-parse-0.7.3", [])
                            Node ("det-parse-0.5.7", [])
                          ])  
                    ])  
                  ]) 
            ])

  version <-- "2.2.0" <!> verifies <*> (versionConstraint <-- "2.2.0 <= v < 2.2.0") |> printf "%A"
  
  // drawTree tree |> println ^ Some 78

  // System.Int32.Parse "01" |> printf "%d"

  // [txt "a"; line; txt "b"; line; txt "c"]
  // |> hcat
  // // |> nest 2
  // |> println ^ Some 78

  0 // return an integer exit code
