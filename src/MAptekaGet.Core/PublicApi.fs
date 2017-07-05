namespace MAptekaGet

module Api =
  open Parsing
  open Utils.Parsing
  open ResultOp

  module NEL = NonEmptyList
  module DR = DependencyResolution

  let rec private addConstraintRes<'err> (upd: Result<Update, 'err>) (constrs: Constraint list) =
    match constrs with
    | []          -> upd
    | constr::cs  -> addConstraintRes (upd <!> addConstraint constr) cs

  let checkUpdates
    (lookupSet: Update list)
    (updatesInput : (string * string list) list) =
    
    match updatesInput with
    | [] -> ResolutionSuccess [] |> Ok
    | upd::upds ->
        NEL.create upd upds
        |> NEL.map (fun (updName, depNames) ->
          depNames
          |> List.map ((<--) dependency)
          |> ResultExt.sequence
          >>= addConstraintRes (update <-- updName)
          <?> BadUpdateFormat
        )
        |> ResultExt.sequenceNonEmpty
        <!> (DR.resolve lookupSet)
    |> (function
        | Ok x    -> ResolutionMessage x
        | Error x -> x
    )

  let publishUpdates
    (lookupSet: Update list)
    (addToStorage: Update -> unit)
    (updatesInput : (string * string list) list) =

    updatesInput
    |> checkUpdates lookupSet
    |> (function
        | ResolutionMessage (ResolutionSuccess acts) as msg ->
            acts
            |> List.map activationUpdate
            |> List.iter addToStorage
            
            msg
        | x -> x
    )

    