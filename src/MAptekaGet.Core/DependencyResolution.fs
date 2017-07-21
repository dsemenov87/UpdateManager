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

  let private candidateTree (initials: NEL<Update>) (allVersions: Update list) : Tree<State> =
    let versionsOf updName =
      allVersions
      |> Seq.filter (fun v -> v.Name = updName)
      |> Seq.sortBy (fun v -> v.Version)
      |> Seq.rev // newest is always better
    
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
    let { Head = head; Tail = tail } =
      NEL.map InitialA initials
    let acts =
      head :: tail
    let initConstraints =
      initials
      |> Seq.collect (fun upd -> upd.Constraints)
      |> Seq.toList
    in
      Node ((head, acts), (buildTree acts (List.allPairs acts initConstraints)))

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

  let private labelInconsistent (state: State) =
    (state, firstConflict state (List.rev (stateConstraints state)))  
    
  let private leaves (Node (root, childs): Tree<'a>) : NEL<'a> =
    let rec calc (tree: Tree<'a>) =
      match tree with
      | Node (a, cs) when Seq.isEmpty cs -> seq [a]
      | Node (_, cs)                     -> Seq.collect calc cs
    in
      match childs |> Seq.collect calc |> Seq.toList with
      | []    -> NEL.create root []
      | x::xs -> NEL.create x xs

  let private dependencyNames =
    List.collect (fun upd -> upd.Constraints)
    >> List.map (fun (Dependency (updateName, dependencyConstraint)) -> updateName)
  
  /// add the number of updates that are missing for a state to become complete as our distance measure.
  let private distance ((_,acts): State) =
    let updates =
      List.map activationUpdate acts
    in
      updates
      |> dependencyNames
      |> List.filter (fun updateName ->
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
  
  let private aggregateToResult
    (allUpdates: Update list)
    (inputList: NEL<(State * Conflict option) * int>) =

    let rec loop primaryConflicts allUpdates inputs =
      match inputs with
      | [] ->
          // printfn "loop: [] -> %A" primaryConflicts;
          MissingUpdateVersion (primaryConflicts |> List.minBy activationUpdate) // todo safe
      
      | (((act,acts), inconsistencyError), levelOfIncompletition)::remains ->
          // printfn "loop: x::xs -> err: %A, level: %A" inconsistencyError levelOfIncompletition;
          step act acts primaryConflicts inconsistencyError levelOfIncompletition remains

    and step act acts primaryConflicts inconsistencyError levelOfIncompletition remains =
      match inconsistencyError with
      | None when levelOfIncompletition > 0 -> // consistant but not complete -> constraint update is not found      
          let rec findNotFounded shift act =
            let upd = activationUpdate act
            match upd.Constraints |> List.tryItem shift with
            | None ->
                activationParent act |> Option.bind (findNotFounded (shift + 1))
            | Some (Dependency (constrUpdName,_)) ->
                Some constrUpdName

          let suggestions =
            nearbyNames (activationUpdate act).Name allUpdates
          
          match findNotFounded 0 act with
          | None ->
              failwith "findNotFounded can't be None when levelOfIncompletition > 0."
          | Some constrUpdName ->
              UpdateNotFound (constrUpdName, Seq.toList suggestions)
            
      | None -> // consistant and complete -> solution
          let upds =
            List.map activationUpdate acts
          
          let rec updateTree upd =
            let childs =
              upd.Constraints
              |> List.collect (fun (Dependency (dn,_)) ->
                List.filter (fun u -> u.Name = dn) upds
              )
            in
              Node (upd, Seq.map updateTree childs)

          acts
          |> List.filter (function InitialA _ -> true | _ -> false)
          |> List.map (updateTree << activationUpdate)
          |> Solution

      | Some (Primary _ as conf) ->
          match primaryConflicts with
          | [] ->
              // printfn "step: Some (Primary _ as conf) -> primaryConflicts: [] -> %A" primaryConflicts;
              loop [act] allUpdates remains
          | confAct::cas ->
              // printfn "step: Some (Primary _ as conf) -> primaryConflicts: same: %b" ((activationParent confAct) = (activationParent act));
              if (activationParent confAct) = (activationParent act)
                then loop (act::confAct::cas) allUpdates remains
                else MissingUpdateVersion confAct
      | Some (Secondary (act1, act2)) ->
          // printfn "step: Some (Secondary (act1, act2)) -> %O\n%O" (activationUpdate act1) (activationUpdate act2);
          IncompatibleConstraints (act1, act2)

    let {Head=(((act,acts), inconsistencyError), levelOfIncompletition);Tail=remains} = inputList
    in
      step act acts [] inconsistencyError levelOfIncompletition remains

  let resolve (allUpdates: Update list) (initials: NEL<Update>) =
    candidateTree initials allUpdates
    |> Tree.map labelInconsistent
    |> pruneTree (Option.isSome << snd)
    |> leaves
    |> NEL.map (fun ((s,_) as i) -> (i, distance s))
    |> NEL.sortBy snd
    |> aggregateToResult allUpdates
      