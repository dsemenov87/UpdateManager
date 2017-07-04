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

  let internal candidateTree (initial: Update) (allVersions: Update list) : Tree<State> =
    let versionsOf updName =
      allVersions
      |> Seq.filter (fun v -> v.Name = updName)
      |> Seq.sortBy (fun v -> v.Version) // older is always better
    
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
      Node ((a, [a]), (buildTree [a] (List.allPairs [a] initial.Constraints)))

  let private stateConstraints ((_,acts): State) : (Activation * Constraint) list =
    let zippedDeps a =
      List.allPairs [a] (activationUpdate a).Constraints
    in
      List.collect zippedDeps acts
  
  let rec internal pruneTree<'a>
    (predicate: 'a -> bool)
    (Node (root, descendants): Tree<'a>) : Tree<'a> =
    
    let nextDescendants =
      if predicate root
        then Seq.empty
        else Seq.map (pruneTree predicate) descendants
    
    Node (root, nextDescendants)
  
  /// finds previous activation of update
  let private findUpd updName ((_,acts): State) =
    List.tryFind (fun a -> (activationUpdate a).Name = updName) acts

  let rec private firstConflict ((act, acts) as state : State) constrActs =
    match constrActs with
    | [] -> None
    | (depAct, (Dependency (updName, disj) as constr)) :: cs ->
        match findUpd updName state with
        | None ->
            firstConflict state cs
        | Some a ->
          let update = activationUpdate a
          if isDisjunctionCompatible update.Version disj
            then firstConflict state cs
            else match activationParent a with
                  | Some parentAct when (activationUpdate parentAct) = (activationUpdate depAct) ->
                      Some (Primary act)
                  | _ ->
                      Some (Secondary (a, ChildA (update, constr, depAct)))

  let internal labelInconsistent (state: State) =
    (state, firstConflict state (List.rev (stateConstraints state)))  
    
  let internal leaves (Node (root, childs): Tree<'a>) : NEL<'a> =
    let rec calc (tree: Tree<'a>) =
      match tree with
      | Node (a, cs) when Seq.isEmpty cs -> seq [a]
      | Node (_, cs)                     -> cs |> Seq.collect calc
    in
      match childs |> Seq.collect calc |> Seq.toList with
      | []    -> NEL.create root []
      | x::xs -> NEL.create x xs

  /// add the number of updates that are missing for a state to become complete as our distance measure.
  let internal distance ((_,acts): State) =
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
  
  let private nearbyNames (target: UpdateName) (allUpdates: Update list) : UpdateName seq =
    allUpdates
    |> Seq.map (fun upd -> (target <--> upd.Name, upd.Name))
    |> Seq.sortBy fst
    |> Seq.map snd
    |> Seq.distinct
    |> Seq.truncate 4
  
  let internal aggregateToResult
    (allUpdates: Update list)
    (inputList: NEL<(State * Conflict option) * int>) =

    let rec loop primaryConflicts allUpdates inputs =
      match inputs with
      | [] ->
          // printfn "loop: [] -> %A" primaryConflicts;
          Error (MissingUpdateVersion (List.sortBy activationUpdate primaryConflicts))
      
      | ((((act,acts), inconsistencyError), levelOfIncompletition)::remains) ->
          // printfn "loop: x::xs -> err: %A, level: %A" inconsistencyError levelOfIncompletition;
          step act acts primaryConflicts inconsistencyError levelOfIncompletition remains

    and step act acts primaryConflicts inconsistencyError levelOfIncompletition remains =
      match inconsistencyError with
      | None when levelOfIncompletition > 0 -> // consistant but not complete -> constraint update is not found 
          let suggestions =
            nearbyNames (activationUpdate act).Name allUpdates
          match stateConstraints (act, acts) with
          | [] -> failwith "stateConstraints can't be empty when levelOfIncompletition > 0."
          | (_,(Dependency (constrUpdName,_)))::_ ->
              Error (UpdateNotFound (act, constrUpdName, Seq.toList suggestions))
            
      | None -> // consistant and complete -> solution
          Ok act
      | Some (Primary _ as conf) ->
          match primaryConflicts with
          | [] ->
              // printfn "step: Some (Primary _ as conf) -> primaryConflicts: [] -> %A" primaryConflicts;
              loop [act] allUpdates remains
          | confAct::cas ->
              // printfn "step: Some (Primary _ as conf) -> primaryConflicts: same: %b" ((activationParent confAct) = (activationParent act));
              if (activationParent confAct) = (activationParent act)
                then loop (act::confAct::cas) allUpdates remains
                else Error (MissingUpdateVersion (confAct::cas))
      | Some (Secondary (act1, act2)) ->
          // printfn "step: Some (Secondary (act1, act2)) -> %O\n%O" (activationUpdate act1) (activationUpdate act2);
          Error (IncompatibleConstraints (act1, act2))

    let {Head=(((act,acts), inconsistencyError), levelOfIncompletition);Tail=remains} = inputList
    in
      step act acts [] inconsistencyError levelOfIncompletition remains

  let resolve (initial: Update) (allUpdates: Update list) =
    candidateTree initial allUpdates
    |> Tree.map labelInconsistent
    |> pruneTree (Option.isSome << snd)
    |> leaves
    |> NEL.map (fun ((s,_) as i) -> (i, distance s))
    |> NEL.sortBy snd
    |> aggregateToResult allUpdates
      