module DependencyResolutionSpecs
  
open Xunit
open FsUnit.Xunit
open MAptekaGet
open MAptekaGet.Domain.Parsing
open MAptekaGet.Utils.Parsing
open MAptekaGet.Utils.ResultOp

module DR = DependencyResolution

  // [ update <-- "ws_client-1.0.0" 
  //     <|*> (dependency <-- "http_client: 1.0.5 <= v <= 1.0.5" <!> addConstraint)
  //     <|*> (dependency <-- "xml_parser: 1.0.0 < v < 1.2.0" <!> addConstraint)
    
  //   update <-- "http_client-1.0.0" 
  //     <|*> (dependency <-- "network: 2.0.0 <= v < 3.0.0" <!> addConstraint)
      
  //   update <-- "http_client-1.0.5" 
  //     <|*> (dependency <-- "network: 2.1.0 <= v < 2.2.0" <!> addConstraint)

  //   update <-- "network-2.0.7" 
      
  //   update <-- "network-2.1.3"

  //   update <-- "xml_parser-1.0.0" 
  //     <|*> (dependency <-- "det_parse: 0.5.0 <= v < 0.6.0" <!> addConstraint)

  //   update <-- "xml_parser-1.2.0" 
  //     <|*> (dependency <-- "det_parse: 0.6.0 <= v < 1.0.0" <!> addConstraint)

  //   update <-- "det_parse-0.5.7"

  //   update <-- "det_parse-0.7.3"
  // ] 

[<Fact>]
let ``can check if dependency version is missing`` () =
  let target =
    update <-- "ws_client-1.0.0" 
        <|*> (dependency <-- "http_client: 1.0.5 <= v <= 1.0.5" <!> addConstraint)
        <|*> (dependency <-- "xml_parser: 1.0.0 < v < 1.2.0"    <!> addConstraint)
  
  let lookupSet =
    [ target
      update <-- "http_client-1.0.0" 
      update <-- "http_client-1.0.2" 
      update <-- "http_client-1.0.3"
    ]
    |> ResultExt.sequence

  let missingError =
    let act1 = 
    Error (MissingUpdateVersion )

  let f =   
    (Ok DR.resolve  <?> ResolutionMessage)
    <*> (target     <?> BadUpdateFormat)
    <*> (lookupSet  <?> BadUpdateFormat)
    |> shouldSucceed
    |> shouldBeFail ()
 
  
  let res = target |> Result.map 
  
  // (target)
  //           //  |> shouldSucceed
  //            |> DR.resolve (shouldSucceed target) 
   
  res <==> Error (MissingUpdateVersion )