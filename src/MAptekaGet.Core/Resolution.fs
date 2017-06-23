namespace MAptekaGet

module internal DependencyResolution =
  type Tree<'a> =
    | Node of 'a * Tree<'a> seq

  type Activation =
    | InitialA  of Update
    | ChildA    of Update * Dependency * Activation

  let private activationUpdate (activation: Activation) : Update =
    match activation with
    | InitialA upd      -> upd
    | ChildA (upd,_,_)  -> upd

  type State = Activation * Activation list

  type Conflict =
    | MAptekaConflict   of Activation
    // the activation of the incompatible update
    | PrimaryConflict   of Activation
    //  the activation containing the original activation of the
    //  update and the activation of the incompatible
    //  dependency constraint
    | SecondaryConflict of Activation * Activation

  let zipRepeat a lst =
    List.zip (List.replicate (lst |> List.length) a) lst
  
  let private stDependencies ((_, acts): State) : (Activation * Dependency) list =
    let zippedDeps a =
      zipRepeat a (activationUpdate a).Dependencies
    in
      List.collect zippedDeps acts
      

  let private dependencies =
    List.collect (fun upd -> upd.Dependencies)
  
  let isConsistent (updates : Update list) : bool =
    updates
    |> dependencies
    |> List.forall (fun (Dependency (updateName, dependencyConstraint)) ->
      updates
      |> List.forall (fun upd ->
        upd.Name <> updateName // || upd.Version satisfy dependencyConstraint 
      ) 
    )

  let isComplete (updates: Update list) : bool =
    updates
    |> dependencies
    |> List.forall (fun (Dependency (updateName, dependencyConstraint)) -> 
      updates
      |> List.filter (fun upd -> upd.Name = updateName)
      |> List.length = 1
    )
  
  /// will remove all nodes and their descendants that match the predicate from a tree
  let prune<'a> : ('a -> bool) -> Tree<'a> -> Tree<'a> =
    fun _ -> id

  let labelInconsistent (updates: Update list) : Update list * bool =
    (updates, isConsistent updates |> not)

  let candidateTree (initial: Update) (allVersions: Update list) : Tree<Activation list> =
    let versionsOf updName =
      List.filter (fun v -> v.Name = updName) allVersions
    
    let multiplyDependencies a deps =
      List.zip (List.replicate (List.length deps) a) deps
    
    let rec buildTree acts deps =
      match deps with
      | (depAct, (Dependency (updName,_) as d)) :: ds ->
        if List.exists ((fun upd -> upd.Name = updName) << activationUpdate) acts then
          buildTree acts ds // skip packages already in tree
        else
          List.map (nodeForVersion acts depAct d ds) (versionsOf updName)
      | _ -> []
    
    and nodeForVersion acts depAct dep deps v =
      let a'=
        ChildA (v, dep, depAct)
      let acts =
        a' :: acts
      let nextDeps =
        v.Dependencies
        |> multiplyDependencies a'
        |> List.append deps
      in
        Node (acts, (buildTree acts nextDeps))
    let a =
      InitialA initial 
    in
      Node ([a], (buildTree [a] (multiplyDependencies a initial.Dependencies)))
    
  let rec mapTree f (Node (a, cs)) =
    Node (f a, Seq.map (mapTree f) cs)  
    
  let rec leaves (tree: Tree<'a>) : 'a seq =
    match tree with
    | Node (a, cs) when Seq.isEmpty cs -> seq { yield a }
    | Node (_, cs)                     -> Seq.collect leaves cs

  let solution (update: Update) : Update list -> Option<Update list> =
    candidateTree update
    >> mapTree labelInconsistent
    >> prune snd
    >> leaves
    >> Seq.map fst
    >> Seq.tryFind isComplete


module ResolutionPrevVersion =
  module MR = MAptekaRestriction
  module VR = VersionRequirement
  
  type ResolutionStack = (UpdateName * VersionInfo) list
  
  type Resolved = Map<UpdateName, UpdateRequirement * ResolutionStack>

  type ResolutionState =
    { Resolved          : Resolved
      Target            : UpdateInfo
      Store             : UpdateInfo Set
      CurrentRestriction: MAptekaRestriction
    }

  [<AutoOpen>]
  module Utils =
    /// Infix version of bindR
    let inline ( >>= ) p f = Result.bind f p
    /// infix version of mapR
    let inline ( <!> ) x f = Result.map f x

    let mapError f = function Error err -> Error (f err) | Ok res -> Ok res
    // let bindError f = function Error err -> match err | Error err1 -> Err(f err) | Ok res -> Ok res
    /// infix version of mapError
    let inline ( <?> ) x f = mapError f x
    let inline ( <??> ) f g x = f x <?> g

    /// Combine two results as "A andThen B"
    let andThen p1 p2 = p1 >>= (fun res1 -> p2 >>= (fun res2 -> Ok (res1, res2) ))
    /// Infix version of andThen
    let ( .>>. ) = andThen

  let internal satisfyVersionNumerationRule
    ({Store=store; Target={FullName=(targetName, targetVersion)}} as state) =
    
    let greaterVersion ((name, version),_) =
      if name = targetName && version > targetVersion then  
        Some (version, targetVersion)
      else
        None

    match store |> Map.toSeq |> Seq.choose greaterVersion |> Seq.tryHead with
    | None          -> Ok state
    | Some (v1, v2) -> Error (v1, v2)
    
  let filterMAptekaRestriction restr state =
    let newRestr =
      state.CurrentRestriction .&& restr

    if newRestr = MAR.empty then
      Error (state.Target.MAptekaRestriction, restr)
    else
      Ok {state with CurrentRestriction = newRestr}

  let doesntRequireThemself
    (requirement: UpdateRequirement)
    ({Target={FullName=(targetName, targetVersion)}} as state) =
    
    if requirement.Name <> targetName then
      Ok state
    else
      Error (targetVersion, requirement.VersionRequirement)

  let filterDependency (requirement: UpdateRequirement) (state: ResolutionState) =
    let satisfyMAptekaRestriction =
      state.Store
      |> Map.toSeq
      |> Seq.filter 
      |> Seq.choose (fun ((name, version),(restr, deps)) ->
        let newRestr =
          state.CurrentRestriction .&& restr
        if name = requirement.Name && newRestr <> MAR.empty then
          Some (name, version, newRestr, deps)
        else
          None
      )

    if satisfyMAptekaRestriction |> Seq.isEmpty then
      Error

    let dependency =
      state.Store
      |> Map.toSeq
      |> Seq.choose (fun ((name, version),value) ->
        if name = requirement.Name && version |> VR.isInRange requirement.VersionRequirement then
          Some (version, value)
        else
          None
      )
      |> Seq.sortBy fst
      // |> Seq.map snd
      |> Seq.rev
      |> Seq.tryHead

    if  <> targetName then
      Ok state
    else
      Error (targetVersion, requirement.VersionRequirement)

  let resolveInDepth (state: ResolutionState) =
    let rec call (requirement: UpdateRequirement) =
      let d =
        state
        |>  filterMAptekaRestriction requirement.MAptekaRestriction   <?>   MAptekaVersionConflict
        >>= (doesntRequireThemself requirement                        <??>  CyclicDependency)
      ()
    ()  
  
  let resolveUpdate (store: UpdateStore)
    (nameRaw: string)
    (versionRaw: string)
    (maptekaRestrictionRaw: string)
    (dependenciesRaw: string) =
    
    let toResolution (((name, version), maptekaRestriction), dependencies) =
      { Applicants = Map.empty
        Store = store
        Target = 
          { FullName            = (name, version)
            MAptekaRestriction  = maptekaRestriction
            Dependencies        = dependencies
          }
        CurrentRestriction = maptekaRestriction
      }
    
    let d =
      nameRaw                      |> UpdateName.parse          <?> InvalidUpdateNameFormat
      .>>. (versionRaw             |> VersionInfo.parse         <?> InvalidVersionFormat)
      .>>. (maptekaRestrictionRaw  |> MAptekaRestriction.parse  <?> InvalidMAptekaRestrictionFormat)
      .>>. (dependenciesRaw        |> DependencySet.parse       <?> InvalidDependenciesFormat)
      <!> toResolution
      >>= (satisfyVersionNumerationRule                         <??> UnexpectedVersion)
      


    ()