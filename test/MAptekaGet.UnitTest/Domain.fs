module DomainSpecs
  
open Xunit
open FsUnit.Xunit
open MAptekaGet.Domain

module VersionInfoSpecs =
  open VersionInfo
  open VersionRange

  [<Fact>]
  let ``can parse version strings and print the result``() = 
    (parse "0.1.2").ToString() |> should equal "0.1.2"
    (parse "1.0.2").ToString() |> should equal "1.0.2"
    (parse "1.2.3").Major |> should equal 1u
    (parse "1.2.3").Minor |> should equal 2u
    (parse "1.2.3").Patch |> should equal 3u

  [<Fact>]
  let ``can compare semvers``() =
    (parse "1.2.3") |> should equal (parse "1.2.3")
    (parse "2.3.4") |> should greaterThan (parse "2.3.3")
    (parse "2.3.4") |> should lessThan (parse "2.3.5")

  [<Fact>]
  let ``version core elements must be non-negative`` () =
    shouldFail (fun () -> parse "1.1.-1" |> ignore)
    shouldFail (fun () -> parse "1.-1.1" |> ignore)
    shouldFail (fun () -> parse "-1.1.1" |> ignore)

  [<Fact>]
  let ``version core elements should accept leading zeroes`` () =
    parse "01.1.1" |> ignore
    parse "1.01.1" |> ignore
    parse "1.1.01" |> ignore

  let isInRange (version:VersionRange) ver =
    version |> includes (Specific (parse ver))

  [<Fact>]
  let ``can check if in range for Specific``() =
    "2.2.0" |> isInRange (Specific (parse "2.2.0")) |> should be True
    "2.4.0" |> isInRange (Specific (parse "2.2.0")) |> should be False
    "2.2.0" |> isInRange (Specific (parse "2.4.0")) |> should be False

  [<Fact>]
  let ``can check if in range for Minimum``() =
    "2.1.0" |> isInRange (Minimum (parse "2.2.0")) |> should be False
    "2.2.0" |> isInRange (Minimum (parse "2.2.0")) |> should be True
    "3.0.0" |> isInRange (Minimum (parse "2.2.0")) |> should be True
    "1.1.0" |> isInRange (Minimum (parse "1.0.0")) |> should be True
    "2.0.3" |> isInRange (atLeast "1.0.0")         |> should be True

  [<Fact>]
  let ``can check if in range for GreaterThan``() =
    "2.1.0" |> isInRange (GreaterThan (parse "2.2.0")) |> should be False
    "2.2.0" |> isInRange (GreaterThan (parse "2.2.0")) |> should be False
    "3.0.0" |> isInRange (GreaterThan (parse "2.2.0")) |> should be True

  [<Fact>]
  let ``can check if in range for Maximum``() =
    "2.0.0" |> isInRange (Maximum (parse "2.2.0")) |> should be True
    "2.2.0" |> isInRange (Maximum (parse "2.2.0")) |> should be True
    "3.0.0" |> isInRange (Maximum (parse "2.2.0")) |> should be False

  [<Fact>]
  let ``can check if in range for LessThan``() =
    "2.0.0" |> isInRange (LessThan (parse "2.2.0"))  |> should be True
    "2.2.0" |> isInRange (LessThan (parse "2.2.0"))  |> should be False
    "3.0.0" |> isInRange (LessThan (parse "2.2.0"))   |> should be False

  [<Fact>]
  let ``can check if in range for Range``() =
    "2.1.0" |> isInRange (Range (Excluding, (parse "2.2.0"), (parse "3.0.0"), Excluding)) |> should be False
    "2.2.0" |> isInRange (Range (Excluding, (parse "2.2.0"), (parse "3.0.0"), Excluding)) |> should be False
    "2.5.0" |> isInRange (Range (Excluding, (parse "2.2.0"), (parse "3.0.0"), Excluding)) |> should be True
    "3.0.0" |> isInRange (Range (Excluding, (parse "2.2.0"), (parse "3.0.0"), Excluding)) |> should be False
    "3.2.0" |> isInRange (Range (Excluding, (parse "2.2.0"), (parse "3.0.0"), Excluding)) |> should be False

    "2.1.0" |> isInRange (Range (Excluding, (parse "2.2.0"), (parse "3.0.0"), Including)) |> should be False
    "2.2.0" |> isInRange (Range (Excluding, (parse "2.2.0"), (parse "3.0.0"), Including)) |> should be False
    "2.5.0" |> isInRange (Range (Excluding, (parse "2.2.0"), (parse "3.0.0"), Including)) |> should be True
    "3.0.0" |> isInRange (Range (Excluding, (parse "2.2.0"), (parse "3.0.0"), Including)) |> should be True
    "3.2.0" |> isInRange (Range (Excluding, (parse "2.2.0"), (parse "3.0.0"), Including)) |> should be False

    "2.1.0" |> isInRange (Range (Including, (parse "2.2.0"), (parse "3.0.0"), Excluding)) |> should be False
    "2.2.0" |> isInRange (Range (Including, (parse "2.2.0"), (parse "3.0.0"), Excluding)) |> should be True
    "2.5.0" |> isInRange (Range (Including, (parse "2.2.0"), (parse "3.0.0"), Excluding)) |> should be True
    "3.0.0" |> isInRange (Range (Including, (parse "2.2.0"), (parse "3.0.0"), Excluding)) |> should be False
    "3.2.0" |> isInRange (Range (Including, (parse "2.2.0"), (parse "3.0.0"), Excluding)) |> should be False

    "2.1.0" |> isInRange (Range (Including, (parse "2.2.0"), (parse "3.0.0"), Including)) |> should be False
    "2.2.0" |> isInRange (Range (Including, (parse "2.2.0"), (parse "3.0.0"), Including)) |> should be True
    "2.5.0" |> isInRange (Range (Including, (parse "2.2.0"), (parse "3.0.0"), Including)) |> should be True
    "3.0.0" |> isInRange (Range (Including, (parse "2.2.0"), (parse "3.0.0"), Including)) |> should be True
    "3.2.0" |> isInRange (Range (Including, (parse "2.2.0"), (parse "3.0.0"), Including)) |> should be False

module MAptekaRestrictionSpecs =
  open MAptekaRestriction
  open VersionInfo

  [<Fact>]
  let ``should filter 4.5.0 and >=4.0.0``() = 
    let l1 = AtLeast (parse "4.0.0")
    let l2 = Exactly (parse "4.5.0")
    l1 .&& l2 |> should equal l2  