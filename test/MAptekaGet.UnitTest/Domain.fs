namespace MAptekaGet.UnitTest

module DomainSpecs =
  open Xunit
  open FsUnit.Xunit
  
  [<Fact>]
  let ``can parse semver strings and print the result``() = 