module DependencyResolution

open FSharp.Control

type State = Activation * Activation list

type Conflict =
  // the activation of the incompatible update
  | Primary   of Activation
  //  the activation containing the original activation of the
  //  update and the activation of the incompatible
  //  dependency constraint
  | Secondary of Activation * Activation

let private candidateTree (initials: NEL<Update>) (versionsOf: UpdateName -> Async<Update Set>) : AsyncTree<State> =
  let rec buildTree acts constrActs =
    asyncSeq {
      match constrActs with
      | [] -> ()
      | (constrAct, constr) :: cs ->
        match constr with
        | Dependency (updName, _) when acts // skip updates already in tree
                                        |> List.map activationUpdate
                                        |> List.exists (fun upd -> upd.Name = updName)
                                        |> not -> 
            let! versions = versionsOf updName
            let versions = versions |> Seq.sort |> Seq.rev  // newest is always better
            for v in versions do
              yield nodeForVersion acts constrAct constr cs v
        | _ ->
            yield! buildTree acts cs
    }
           
  and nodeForVersion acts constrAct constr cs v =
    let a' =
      ChildA (v, constr, constrAct)
    let nextConstrs =
      (List.allPairs [a'] v.Constraints) @ cs
    let head = (a', a'::acts)
    let tail = buildTree (a'::acts) nextConstrs
    let res = ANode (head, tail)
    in
      res

  let { Head = head; Tail = tail } =
    NEL.map InitialA initials
  
  let acts = head :: tail
  
  let initConstraints =
    initials
    |> Seq.collect (fun upd -> upd.Constraints)
    |> Seq.toList
  in
    ANode ((head, acts), (buildTree acts (List.allPairs acts initConstraints)))

let private stateConstraints ((_,acts): State) : (Activation * Constraint) list =
  let zippedDeps a =
    List.allPairs [a] (activationUpdate a).Constraints
  in
    List.collect zippedDeps acts

let rec private pruneTree<'a>
  (predicate: 'a -> bool)
  (ANode (root, descendants): AsyncTree<'a>) : AsyncTree<'a> =
  
  let nextDescendants =
    if predicate root
      then AsyncSeq.empty
      else AsyncSeq.map (pruneTree predicate) descendants
  in
    ANode (root, nextDescendants)

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
                    let (Dependency (_,{Head=(Range (lowestVersion,_,_,_))})) = constr
                    let fakeUpdate = // to show in tree
                      {Name=updName;Constraints=[];Version=lowestVersion}
                    Some (Secondary (a, ChildA (fakeUpdate, constr, depAct)))

let private labelInconsistent (state: State) =
  (state, firstConflict state (List.rev (stateConstraints state)))  
  
let private leaves (ANode (root, childs): AsyncTree<'a>) : Async<NEL<'a>> =
  async {
    let rec calc (ANode (a, cs): AsyncTree<'a>) =
      asyncSeq {
        let! maybeHead = AsyncSeq.tryFirst cs
        match maybeHead with
        | Some head ->
            yield! AsyncSeq.collect calc (AsyncSeq.cons head cs)
        | None ->
            yield a
      }
    
    // do! childs |> AsyncSeq.toListAsync |> Async.map ^ printfn "leaves %A" |> Async.Ignore
    let! res = childs |> AsyncSeq.collect calc |> AsyncSeq.toListAsync
    match res with
    | []    -> return NEL.create root []
    | x::xs -> return NEL.create x xs
  }

let private dependencyNames =
  List.collect (fun upd -> upd.Constraints)
  >> List.map (fun (Dependency (updateName, dependencyConstraint)) -> updateName)

/// add the number of updates that are missing for a state to become complete as our distance measure.
let private distance ((act,acts): State) =
  let updates =
    List.map activationUpdate acts
  in
    updates
    // |> (fun x -> printfn "List.map1: %A" (List.map updName x);x)
    |> dependencyNames
    // |> (fun x -> printfn "List.map2: %A" x;x)
    |> List.filter (fun updateName ->
      updates
      |> List.exists (fun upd -> upd.Name = updateName)
      |> not
    )
    |> List.length

let private aggregateToResult (inputList: NEL<(State * Conflict option) * int>) =
  let rec loop primaryConflicts inputs =
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

        match findNotFounded 0 act with
        | None ->
            failwith "findNotFounded can't be None when levelOfIncompletition > 0."
        | Some constrUpdName ->
            UpdateNotFound (constrUpdName, []) // suggestions will be added in outer procedure - for simplicity
          
    | None -> // consistant and complete -> solution
        let upds =
          List.map activationUpdate acts

        let dependencies = dependencyNames upds
        
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
        |> List.map activationUpdate
        |> List.filter (fun upd -> List.forall ((<>) upd.Name) dependencies)
        |> List.map updateTree
        |> Solution

    | Some (Primary _ as conf) ->
        match primaryConflicts with
        | [] ->
            // printfn "step: Some (Primary _ as conf) -> primaryConflicts: [] -> %A" primaryConflicts;
            loop [act] remains
        | confAct::cas ->
            // printfn "step: Some (Primary _ as conf) -> primaryConflicts: same: %b" ((activationParent confAct) = (activationParent act));
            if (activationParent confAct) = (activationParent act)
              then loop (act::confAct::cas) remains
              else MissingUpdateVersion confAct
    
    | Some (Secondary (act1, act2)) ->
        // printfn "step: Some (Secondary (act1, act2)) -> %O\n%O" (activationUpdate act1) (activationUpdate act2);
        IncompatibleConstraints (act1, act2)

  let {Head=(((act,acts), inconsistencyError), levelOfIncompletition);Tail=remains} = inputList
  in
    step act acts [] inconsistencyError levelOfIncompletition remains

let resolve (versionsOf: UpdateName -> Async<Update Set>) (initials: NEL<Update>) =
  candidateTree initials versionsOf
  |> AsyncTree.map labelInconsistent
  |> pruneTree (Option.isSome << snd)
  |> leaves
  |> Async.map (NEL.sortBy snd << NEL.map (fun ((s,_) as i) -> (i, distance s)))
  |> Async.map ^ aggregateToResult
    