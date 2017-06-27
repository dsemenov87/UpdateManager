namespace MAptekaGet

module DependencyResolution =
  module VC = VersionConstraint
  
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

  let candidateTree (initial: Update) (allVersions: Update list) : Tree<State> =
    let versionsOf updName =
      List.filter (fun v -> v.Name = updName) allVersions
    
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
 
  let private isComplete ((_,acts): State) : bool =
    let updates =
      List.map activationUpdate acts
    in
      updates
      |> List.collect (fun upd -> upd.Constraints)
      |> List.choose (function Dependency (updateName, dependencyConstraint) -> Some updateName | _ -> None)
      |> List.forall (fun updateName ->
        updates
        |> List.filter (fun upd -> upd.Name = updateName)
        |> List.length = 1
      )
  
  /// will remove all nodes and their descendants that match the predicate from a tree
  let rec private prune<'a>
    (predicate: 'a -> bool)
    (Node (root, descendants): Tree<'a>) : Tree<'a> seq =
    
    seq {
      if predicate root
        then yield Node (root, Seq.collect (prune predicate) descendants)
    }
  
  /// finds previous activation of update
  let private findUpd updName ((_,acts): State) =
    acts
    |> List.map activationUpdate
    |> List.tryFind (fun upd -> upd.Name = updName)

  /// finds current mapteka restriction
  let private findMapteka ((_,acts): State) =
    acts
    |> List.collect ((fun upd -> upd.Constraints) << activationUpdate)
    |> List.choose (function MApteka vdisj -> Some vdisj | _ -> None)
    |> List.reduce (.&&)

  let rec firstConflict ((act, acts) as state : State) constrActs =
    match constrActs with
    | [] -> None
    | (depAct, constr) :: ds ->
      match constr with
      | MApteka vdisj ->
          if VC.isDisjunctionCompatible maptekaVersion vdisj then
            firstConflict state ds maptekaVersion
          else
            Some (MAptekaConflict act)
      
      | Dependency (updName, disj) as d ->
          match findPrevActivation updName act with
          | None ->
              firstConflict state ds maptekaVersion
          | Some a ->
          if (activationUpdate a).IsCompatibleTo disj then
            firstConflict state ds maptekaVersion
          elif activationParent a = Some depAct then
            Some (PrimaryConflict act)
          else
            Some (SecondaryConflict (act, depAct))

  let labelInconsistent maptekaVersion (state: State) : State * Conflict option =
    (state, firstConflict state (List.rev (stDependencies state)) maptekaVersion)
    
  let rec mapTree f (Node (a, cs)) =
    Node (f a, Seq.map (mapTree f) cs)  
    
  let rec leaves (tree: Tree<'a>) : 'a seq =
    match tree with
    | Node (a, cs) when Seq.isEmpty cs -> seq { yield a }
    | Node (_, cs)                     -> Seq.collect leaves cs

  let solution : Tree<State> -> Option<State> =
    mapTree labelInconsistent
    >> prune (Option.isSome << snd)
    >> leaves
    >> Seq.map fst
    >> Seq.tryFind isComplete
