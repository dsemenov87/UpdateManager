namespace MAptekaGet

module DependencyResolution =
  open Dist
  type NEL<'a> = NonEmptyList<'a>
  module NEL = NonEmptyList

  type State = Activation * Activation list

  type Conflict =
    // the activation of the incompatible update
    | Primary   of Activation
    //  the activation containing the original activation of the
    //  update and the activation of the incompatible
    //  dependency constraint
    | Secondary of Activation * Activation

  let private candidateTree (initial: Update) (allVersions: Update list) : Tree<State> =
    let versionsOf updName =
      allVersions
      |> Seq.filter (fun v -> v.Name = updName)
      |> Seq.sortBy (fun v -> v.Version)
      |> Seq.rev  // newer is always better
    
    let rec buildTree acts constrActs =
      seq {
        match constrActs with
        | [] -> ()
        | (constrAct, constr) :: cs ->
          match constr with
          | Dependency (updName, _) when acts // skip updates already in tree
                                          |> List.map activationUpdate
                                          |> List.exists (fun upd -> upd.Name = updName)
                                          |> not -> 
              yield! Seq.map (nodeForVersion acts constrAct constr cs) (versionsOf updName)
          | _ ->
              yield! buildTree acts cs
      }
             
    and nodeForVersion acts constrAct constr cs v =
      let a' =
        ChildA (v, constr, constrAct)
      let nextConstrs =
        (List.allPairs [a'] v.Constraints) @ cs
      in
        Node ((a', a'::acts), (buildTree (a'::acts) nextConstrs))
    let a =
      InitialA initial 
    in
      Node ((a, []), (buildTree [a] (List.allPairs [a] initial.Constraints)))

  let private stateConstraints ((_,acts): State) : (Activation * Constraint) list =
    let zippedDeps a =
      List.allPairs [a] (activationUpdate a).Constraints
    in
      List.collect zippedDeps acts
  
  let rec private pruneTree<'a>
    (predicate: 'a -> bool)
    (Node (root, descendants): Tree<'a>) : Tree<'a> =
    
    let nextDescendants =
      if predicate root
        then Seq.map (pruneTree predicate) descendants
        else Seq.empty
    
    Node (root, nextDescendants)
  
  /// finds previous activation of update
  let private findUpd updName ((_,acts): State) =
    List.tryFind (fun a -> (activationUpdate a).Name = updName) acts

  let rec private firstConflict ((act, acts) as state : State) constrActs =
    match constrActs with
    | [] -> None
    | (depAct, Dependency (updName, disj) as d) :: cs ->
        match findUpd updName state with
        | None ->
            firstConflict state cs
        | Some a ->
          if isDisjunctionCompatible (activationUpdate a).Version disj then
            firstConflict state cs
          else match activationParent a with
                | Some depAct -> Some (Primary act)
                | _           -> Some (Secondary (act, depAct))

  let private labelInconsistent (state: State) =
    (state, firstConflict state (List.rev (stateConstraints state)))  
    
  let private leaves (Node (root, childs): Tree<'a>) : NEL<'a> =
    let rec calc (tree: Tree<'a>) =
      match tree with
      | Node (a, cs) when Seq.isEmpty cs -> [a]
      | Node (_, cs)                     -> cs |> Seq.collect calc |> Seq.toList
    in
      NEL.create root (childs |> Seq.collect calc |> Seq.toList)

  /// add the number of updates that are missing for a state to become complete as our distance measure.
  let private distance ((_,acts): State) =
    let updates =
      List.map activationUpdate acts
    in
      updates
      |> List.collect (fun upd -> upd.Constraints)
      |> List.filter (fun (Dependency (updateName, dependencyConstraint)) ->
        updates
        |> List.exists (fun upd -> upd.Name = updateName)
        |> not
      )
      |> List.length
  
  let private nearbyNames (target: UpdateName) (allUpdates: Update list) : Update seq =
    let ratedNames =
      Seq.map (fun upd -> (target <--> upd.Name, upd)) allUpdates
    let sortedNames =
      Seq.sortBy fst ratedNames
    in
      sortedNames |> Seq.take 4 |> Seq.map snd
  
  let private aggregateToResult
    (primaryConflicts: Activation list)
    (allUpdates: Update list)
    (inputList: NEL<(State * Conflict option) * int>) =

    let rec loop primaryConflicts allUpdates inputs =
      match inputs with
      | [] ->
          Error (MissingUpdateVersion primaryConflicts)
      
      | ((((act,acts), inconsistencyError), levelOfIncompletition)::remains) ->
          step act acts inconsistencyError levelOfIncompletition remains

    and step act acts inconsistencyError levelOfIncompletition remains =
      match inconsistencyError with
      | None when levelOfIncompletition > 0 -> // consistant but not complete -> constraint update is not found 
          let suggestions =
            nearbyNames (activationUpdate act).Name allUpdates
          in
            Error (UpdateNotFound (act, Seq.toList suggestions))
      | None -> // consistant and complete -> solution
          Ok act
      | Some (Primary _ as conf) ->
          match primaryConflicts with
          | [] ->
              loop [act] allUpdates remains
          | confAct::cas ->
              if (activationParent confAct) = (activationParent act)
                then loop (act::confAct::cas) allUpdates remains
                else Error (MissingUpdateVersion (confAct::cas))
      | Some (Secondary _ as conf) ->
          Error (IncompatibleConstraints act)

    let {Head=(((act,acts), inconsistencyError), levelOfIncompletition);Tail=remains} = inputList
    in
      step act acts inconsistencyError levelOfIncompletition remains

  let resolve (initial: Update) (allUpdates: Update list) =
    candidateTree initial allUpdates
    |> Tree.map labelInconsistent
    |> pruneTree (Option.isSome << snd)
    |> leaves
    |> NEL.map (fun ((s,_) as i) -> (i, distance s))
    |> NEL.sortBy snd
    |> aggregateToResult [] allUpdates
      