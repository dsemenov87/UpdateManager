module DomainSpecs
  
open Xunit
open FsUnit.Xunit
open Domain.Parsing
open Prelude.Parsing
open Prelude.ChoiceInfixes

module VersionSpecs =
  
  [<Fact>]
  let ``can parse version strings and print the result``() = 
    (version <-- "0.1.2") <!> string |> shouldBeSucceed "0.1.2"
    ((version <-- "1.2.3") |> shouldSucceed).Major |> shouldStrictEquals 1u
    ((version <-- "1.2.3") |> shouldSucceed).Minor |> shouldStrictEquals 2u
    ((version <-- "1.2.3") |> shouldSucceed).Patch |> shouldStrictEquals 3u

  [<Fact>]
  let ``can compare semvers``() =
    version <-- "1.2.3" <!> should equal        <*> (version <-- "1.2.3") |> ignore
    version <-- "2.3.3" <!> should greaterThan  <*> (version <-- "2.3.4") |> ignore
    version <-- "2.3.5" <!> should lessThan     <*> (version <-- "2.3.4") |> ignore

  [<Fact>]
  let ``version core elements must be non-negative`` () =
    version <-- "1.1.-1" |> shouldFail |> ignore
    version <-- "1.-1.1" |> shouldFail |> ignore
    version <-- "-1.1.1" |> shouldFail |> ignore

  [<Fact>]
  let ``version core elements should accept leading zeroes`` () =
    version <-- "01.1.1" <!> should equal <*> (version <-- "1.1.1") |> ignore
    version <-- "1.01.1" <!> should equal <*> (version <-- "1.1.1") |> ignore
    version <-- "1.1.01" <!> should equal <*> (version <-- "1.1.1") |> ignore

  [<Fact>]
  let ``can check if in range for 'exact'``() =
    version <-- "2.2.0" <!> verifies <*> (versionConstraint <-- "2.2.0 <= v <= 2.2.0") <==> true
    version <-- "2.4.0" <!> verifies <*> (versionConstraint <-- "2.2.0 <= v <= 2.2.0") <==> false
    version <-- "2.2.0" <!> verifies <*> (versionConstraint <-- "2.4.0 <= v <= 2.4.0") <==> false

  [<Fact>]
  let ``can check if in range for 'minimum'``() =
    version <-- "2.1.0" <!> verifies <*> (versionConstraint <-- "2.2.0 <= v < 3.0.0") <==> false
    version <-- "2.2.0" <!> verifies <*> (versionConstraint <-- "2.2.0 <= v < 3.0.0") <==> true
    version <-- "3.0.0" <!> verifies <*> (versionConstraint <-- "2.2.0 <= v < 3.0.1") <==> true
    version <-- "1.1.0" <!> verifies <*> (versionConstraint <-- "1.1.0 <= v < 2.0.0") <==> true
    version <-- "2.0.3" <!> verifies <*> (versionConstraint <-- "1.1.0 < v < 3.0.0") <==> true

  [<Fact>]
  let ``can check if in range for 'greater than'``() =
    version <-- "2.1.0" <!> verifies <*> (versionConstraint <-- "2.2.0 < v <= 3.0.0") <==> false
    version <-- "2.2.0" <!> verifies <*> (versionConstraint <-- "2.2.0 < v <= 3.0.0") <==> false
    version <-- "3.0.0" <!> verifies <*> (versionConstraint <-- "2.2.0 < v <= 3.0.0") <==> true
    version <-- "3.0.0" <!> verifies <*> (versionConstraint <-- "2.2.0 < v < 3.0.1") <==> true

  [<Fact>]
  let ``can check if in range for 'maximum'``() =
    version <-- "2.0.0" <!> verifies <*> (versionConstraint <-- "1.0.0 <= v <= 2.2.0") <==> true
    version <-- "2.2.0" <!> verifies <*> (versionConstraint <-- "1.0.0 <= v <= 2.2.0") <==> true
    version <-- "3.0.0" <!> verifies <*> (versionConstraint <-- "1.0.0 <= v <= 2.2.0") <==> false

  [<Fact>]
  let ``can check if in range for 'less than'``() =
    version <-- "2.0.0" <!> verifies <*> (versionConstraint <-- "1.0.0 <= v < 2.2.0") <==> true
    version <-- "2.2.0" <!> verifies <*> (versionConstraint <-- "1.0.0 <= v < 2.2.0") <==> false
    version <-- "3.0.0" <!> verifies <*> (versionConstraint <-- "1.0.0 <= v < 2.2.0") <==> false

  [<Fact>]
  let ``can check if in range for 'between'``() =
    version <-- "2.1.0" <!> verifies <*> (versionConstraint <-- "2.2.0 < v < 3.0.0") <==> false
    version <-- "2.2.0" <!> verifies <*> (versionConstraint <-- "2.2.0 < v < 3.0.0") <==> false
    version <-- "2.5.0" <!> verifies <*> (versionConstraint <-- "2.2.0 < v < 3.0.0") <==> true
    version <-- "3.0.0" <!> verifies <*> (versionConstraint <-- "2.2.0 < v < 3.0.0") <==> false
    version <-- "3.2.0" <!> verifies <*> (versionConstraint <-- "2.2.0 < v < 3.0.0") <==> false

    version <-- "2.1.0" <!> verifies <*> (versionConstraint <-- "2.2.0 < v <= 3.0.0") <==> false
    version <-- "2.2.0" <!> verifies <*> (versionConstraint <-- "2.2.0 < v <= 3.0.0") <==> false
    version <-- "2.5.0" <!> verifies <*> (versionConstraint <-- "2.2.0 < v <= 3.0.0") <==> true
    version <-- "3.0.0" <!> verifies <*> (versionConstraint <-- "2.2.0 < v <= 3.0.0") <==> true
    version <-- "3.2.0" <!> verifies <*> (versionConstraint <-- "2.2.0 < v <= 3.0.0") <==> false
    
    version <-- "2.1.0" <!> verifies <*> (versionConstraint <-- "2.2.0 <= v < 3.0.0") <==> false
    version <-- "2.2.0" <!> verifies <*> (versionConstraint <-- "2.2.0 <= v < 3.0.0") <==> true
    version <-- "2.5.0" <!> verifies <*> (versionConstraint <-- "2.2.0 <= v < 3.0.0") <==> true
    version <-- "3.0.0" <!> verifies <*> (versionConstraint <-- "2.2.0 <= v < 3.0.0") <==> false
    version <-- "3.2.0" <!> verifies <*> (versionConstraint <-- "2.2.0 <= v < 3.0.0") <==> false

    version <-- "2.1.0" <!> verifies <*> (versionConstraint <-- "2.2.0 <= v <= 3.0.0") <==> false
    version <-- "2.2.0" <!> verifies <*> (versionConstraint <-- "2.2.0 <= v <= 3.0.0") <==> true
    version <-- "2.5.0" <!> verifies <*> (versionConstraint <-- "2.2.0 <= v <= 3.0.0") <==> true
    version <-- "3.0.0" <!> verifies <*> (versionConstraint <-- "2.2.0 <= v <= 3.0.0") <==> true
    version <-- "3.2.0" <!> verifies <*> (versionConstraint <-- "2.2.0 <= v <= 3.0.0") <==> false

  [<Fact>]
  let ``can parse updates and print the result``() =
    update <-- "network-2.0.7"    <!> string |> shouldBeSucceed "network-2.0.7"
    update <-- "network-2.1.3"    <!> string |> shouldBeSucceed "network-2.1.3"
    update <-- "xml_parser-1.0.0" <!> string |> shouldBeSucceed "xml_parser-1.0.0"


module VersionCheckSpecs =
  let newUpdate name major minor patch =
    { Name        = UpdateName name
      Version     = {Major=major; Minor=minor; Patch=patch;}
      Constraints = []
    }

  
  let ``common_nskPricingCheck-0.32.0`` = newUpdate "common_nskPricingCheck" 0u 32u 0u
  let ``common_nskPricingCheck-0.33.0`` = newUpdate "common_nskPricingCheck" 0u 33u 0u
  let ``common_nskPricingCheck-0.34.0`` = newUpdate "common_nskPricingCheck" 0u 34u 0u
  let ``common_nskPricingCheck-0.35.0`` = newUpdate "common_nskPricingCheck" 0u 35u 0u

  let lookupSet =
    [ ``common_nskPricingCheck-0.33.0``
      ``common_nskPricingCheck-0.34.0``
    ]

  [<Fact>]
  let ``if version exists then return AlreadyPublished`` () =
    match ``common_nskPricingCheck-0.34.0``
          |> checkUpdateVersion lookupSet with
    | AlreadyPublished _  -> ()
    | problem           -> failwith ""

  [<Fact>]
  let ``if version doesn't exists but less then previous should return UnexpectedVersion`` () =
    match ``common_nskPricingCheck-0.32.0``
          |> checkUpdateVersion lookupSet with
    | UnexpectedVersion _  -> ()
    | problem           -> failwith ""

  [<Fact>]
  let ``if version greater then previous should succeed`` () =
    match ``common_nskPricingCheck-0.35.0``
          |> checkUpdateVersion lookupSet with
    | CorrectVersion _  -> ()
    | problem           -> failwith ""
    
  

    
    

  
