namespace MAptekaGet

module DependencyResolution =
  type Tree<'a> =
    | Node of 'a * Tree<'a> seq

  let private activationUpdate (activation: Activation) : Update =
    match activation with
    | InitialA upd      -> upd
    | ChildA (upd,_,_)  -> upd

  let private activationParent (activation: Activation) =
    match activation with
    | InitialA _            -> None
    | ChildA (_,_,parent)   -> Some parent

  type State = Activation * Activation list

  let private candidateTree (initial: Update) (allVersions: Update Set) : Tree<State> =
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
  
  let rec private prune<'a>
    (predicate: 'a -> bool)
    (Node (root, descendants): Tree<'a>) : Tree<'a> =
    
    let nextDescendants =
      if predicate root
        then Seq.map (prune predicate) descendants
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
                | Some depAct -> Some (PrimaryConflict act)
                | _           -> Some (SecondaryConflict (act, depAct))

  let private labelInconsistent (state: State) =
    (state, firstConflict state (List.rev (stateConstraints state)))
    
  let rec private mapTree f (Node (a, cs)) =
    Node (f a, Seq.map (mapTree f) cs)  
    
  let rec private leaves (tree: Tree<'a>) : 'a seq =
    match tree with
    | Node (a, cs) when Seq.isEmpty cs -> seq { yield a }
    | Node (_, cs)                     -> Seq.collect leaves cs

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
  
  let private solution =
    mapTree labelInconsistent
    >> prune (Option.isSome << snd)
    >> leaves
    >> Seq.map (fun ((s,_) as i) -> (i, distance s))
    >> Seq.sortBy snd
    >> Seq.head
  
  let resolve (initial: Update) (allVersions: LookupSet) =
    candidateTree initial allVersions
    |> solution
    |> (fun (((act,acts), err), levelOfCompletition) ->
      if levelOfCompletition > 0
        then Error (PrimaryConflict act)
        else match err with
              | None ->
                  acts
                  |> List.map ((fun upd -> upd.Name, upd) << activationUpdate)
                  |> Map.ofList
                  |> Ok
              | Some conf ->
                  Error conf
    )   