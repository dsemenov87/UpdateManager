module DependencyResolutionSpecs
  
open Xunit
open FsUnit.Xunit
open MAptekaGet
open CommonUtils

module DR = DependencyResolution
module NEL = NonEmptyList

[<Fact>]
let ``can check if dependency version is missing`` () =
  let ``1.0.5 <= http_client <= 1.0.5`` =
    dep "http_client: 1.0.5 <= v <= 1.0.5"

  let ``ws_client-1.0.0`` =
    upd "ws_client-1.0.0" |> addConstraint ``1.0.5 <= http_client <= 1.0.5`` 

  let childA x =
    ChildA (x, ``1.0.5 <= http_client <= 1.0.5``, InitialA ``ws_client-1.0.0``)
  
  let lookupSet =
    [ ``ws_client-1.0.0``
      upd "http_client-1.0.0" 
      upd "http_client-1.0.2"
      upd "http_client-1.0.3"
    ]

  let acts =
    [ "http_client-1.0.0" 
      "http_client-1.0.2"
      "http_client-1.0.3"
    ]
    |> List.map (childA << upd)
 
  DR.resolve lookupSet (NEL.singleton ``ws_client-1.0.0``)
  |> shouldStrictEquals (MissingUpdateVersion acts)
  
[<Fact>]
let ``can check if dependency is not found`` () =
  let ``1.0.5 <= http_cilent <= 1.0.5`` =
    dep "http_cilent: 1.0.5 <= v <= 1.0.5"
  
  let ``ws_client-1.0.0`` =
    upd "ws_client-1.0.0" |> addConstraint ``1.0.5 <= http_cilent <= 1.0.5``
  
  let lookupSet =
    [ ``ws_client-1.0.0``
      upd "http_client-1.0.0" 
      upd "http_client-1.0.2"
      upd "http_client-1.0.3"
    ]

  let suggestions =
    [ UpdateName "ws_client"
      UpdateName "http_client"
    ]

  let expected =
    UpdateNotFound (InitialA ``ws_client-1.0.0``, UpdateName "http_cilent", suggestions)

  DR.resolve lookupSet (NEL.singleton ``ws_client-1.0.0``)
  |> shouldStrictEquals expected

[<Fact>]
let ``can check if constraints are incompatible`` () =
  let ``rest_client-1.0.0`` =
    upd "rest_client-1.0.0"
    |> addConstraint ^ dep "http_client: 1.0.1 <= v <= 1.0.1"
    |> addConstraint ^ dep "json: 1.0.0 <= v <= 1.0.0"

  let lookupSet = 
    [ ``rest_client-1.0.0``
      upd "http_client-1.0.1" |> addConstraint ^ dep "network: 1.2.2 <= v <= 1.2.2"
      upd "network-1.2.2"     |> addConstraint ^ dep "bytes: 1.0.5 <= v < 2.0.0"
      upd "network-2.0.1"
      upd "json-1.0.0"        |> addConstraint ^ dep "bytes: 1.0.0 <= v <= 1.0.0"
      upd "bytes-1.0.0"
      upd "bytes-1.0.7"
    ]

  let a1 =
    ChildA (  upd "bytes-1.0.0",
              dep "bytes: 1.0.5 <= v < 2.0.0",
              ChildA (  upd "network-1.2.2",
                        dep "network: 1.2.2 <= v <= 1.2.2",
                        ChildA (  upd "http_client-1.0.1",
                                  dep "http_client: 1.0.1 <= v <= 1.0.1",
                                  InitialA ( ``rest_client-1.0.0`` ))))

  let a2 =
    ChildA (  upd "bytes-1.0.0",
              dep "bytes: 1.0.0 <= v <= 1.0.0",
              ChildA (  upd "json-1.0.0",
                        dep "json: 1.0.0 <= v <= 1.0.0",
                        InitialA ( ``rest_client-1.0.0`` )))

  in
    DR.resolve lookupSet (NEL.singleton ``rest_client-1.0.0``)
    |> shouldStrictEquals (IncompatibleConstraints (a2, a1))

[<Fact>]
let ``if solution exists then returns it't activation`` =
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

  let a =
    ChildA (  upd "bytes-1.0.0",
              dep "bytes: 1.0.5 <= v < 2.0.0",
              ChildA (  upd "network-1.2.2",
                        dep "network: 1.2.2 <= v <= 1.2.2",
                        ChildA (  upd "http_client-1.0.1",
                                  dep "http_client: 1.0.1 <= v <= 1.0.1",
                                  InitialA ( ``rest_client-0.9.9`` ))))

  in
    DR.resolve lookupSet (NEL.singleton ``rest_client-0.9.9``)
    |> shouldStrictEquals (ResolutionSuccess [ InitialA ``rest_client-0.9.9``])