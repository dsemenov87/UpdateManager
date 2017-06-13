module internal MAptekaGet.DependencyResolution

open System
open System.Text
open System.Collections.Generic
open MAptekaGet.Domain
open MAptekaGet.Utils

type UpdateDetails =
  { Name               : UpdateName
    DownloadLink       : string
    Unlisted           : bool
    DirectDependencies : DependencySet
  }

type ResolvedUpdate =
  { Name                : UpdateName
    Version             : VersionInfo
    MAptekaRestriction  : MAptekaRestriction
    Dependencies        : DependencySet
    Unlisted            : bool
  } 
  override this.ToString () = sprintf "%O %O" this.Name this.Version

type UpdateResolution = Map<UpdateName, ResolvedUpdate>

module DependencySetFilter =
  let isIncluded (restriction:MAptekaRestriction) (dependency:UpdateName * VersionRequirement * MAptekaRestriction) =
    match dependency with
    | _, _, vr when vr = MAptekaRestriction.noRestriction -> true
    | _, _, mar -> mar |> MAptekaRestriction.isSubsetOf restriction

  let filterByRestrictions (restriction:MAptekaRestriction) (dependencies:DependencySet) : DependencySet =
    if restriction = MAptekaRestriction.noRestriction then
      dependencies
    else
      dependencies
      |> Set.filter (isIncluded restriction)

  let isUpdateCompatible (dependencies:DependencySet) (update:ResolvedUpdate) : bool =
    dependencies
    // exists any not matching stuff
    |> Seq.exists (fun (name, requirement, restriction) ->
      if name = update.Name && not (requirement |> VersionRequirement.isInRange update.Version) then
        printf "   Incompatible dependency: %O %O conflicts with resolved version %O" name requirement update.Version
        true
      else false
    )
    |> not // then we are not compatible

let cleanupNames (model : UpdateResolution) : UpdateResolution =
  model
  |> Map.map (fun _ upd ->
    { upd
      with
        Dependencies =
          upd.Dependencies
          |> Set.map (fun (name, v, d) -> model.[name].Name, v, d)
    }) 

type ResolverStep =
  { Relax: bool
    FilteredVersions : Map<UpdateName, VersionInfo list>
    CurrentResolution : Map<UpdateName,ResolvedUpdate>;
    ClosedRequirements : Set<UpdateRequirement>
    OpenRequirements : Set<UpdateRequirement>
  }

[<RequireQualifiedAccess>]
type Resolution =
  | Ok of UpdateResolution
  | Conflict of resolveStep       : ResolverStep
              * requirementSet    : UpdateRequirement Set
              * requirement       : UpdateRequirement
              * getUpdateVersions : (UpdateName -> VersionInfo seq)

let getResolutionConflicts (res:Resolution) =
    match res with
    | Resolution.Ok _ -> Set.empty
    | Resolution.Conflict (currentStep,_,lastUpdateRequirement,_) ->
        currentStep.ClosedRequirements
        |> Set.union currentStep.OpenRequirements
        |> Set.add lastUpdateRequirement
        |> Set.filter (fun x -> x.Name = lastUpdateRequirement.Name)

let buildResolutionConflictReport (errorReport:StringBuilder)  (conflicts:UpdateRequirement Set) =
  let formatVR (vr:VersionRequirement) =
    vr.ToString ()
    |> fun s -> if String.IsNullOrWhiteSpace s then ">= 0" else s

  match conflicts with
  | s when s.IsEmpty -> errorReport
  | conflicts ->
    errorReport.AppendLine (sprintf "  Conflict detected:") |> ignore

    let getConflictMessage req =
      let vr = formatVR req.VersionRequirement
      sprintf "   - Requested update %O: %s" req.Name vr
    
    conflicts
    |> Seq.fold (fun (errorReport:StringBuilder) conflict ->
      errorReport.AppendLine (getConflictMessage conflict)) errorReport

let getResolutionErrorText showResolvedUpdates = function
  | Resolution.Ok _ -> ""
  | Resolution.Conflict (currentStep,_,_,getVersionF) as res ->
    let errorText =
      if showResolvedUpdates && not currentStep.CurrentResolution.IsEmpty then
        ( StringBuilder().AppendLine  "  Resolved updates:"
        , currentStep.CurrentResolution)
        ||> Map.fold (fun sb _ resolvedUpdate ->
            sb.AppendLine (sprintf "   - %O %O" resolvedUpdate.Name resolvedUpdate.Version))
      else StringBuilder()

    match getResolutionConflicts res with
    | c when c.IsEmpty  ->
      errorText.AppendLine(sprintf
        "  Could not resolve update %O. Unknown resolution error."
          (Seq.head currentStep.OpenRequirements))
    | cfs when cfs.Count = 1 ->
      let c = cfs.MinimumElement
      let errorText = buildResolutionConflictReport errorText cfs
      match getVersionF c.Name |> Seq.toList with
      | [] -> errorText.AppendLine(sprintf  "   - No versions available.")
      | avalaibleVersions ->
        ( errorText.AppendLine(sprintf  "   - Available versions:")
        , avalaibleVersions )
        ||> List.fold (fun sb elem -> sb.AppendLine(sprintf "     - %O" elem))
    | conflicts -> buildResolutionConflictReport errorText conflicts
    |> string

let getResolutionModelOrFail = function
  | Resolution.Ok model -> model
  | Resolution.Conflict _ as res ->
    failwithf  "There was a version conflict during update resolution.\n\
                %s\n  Please try to relax some conditions or resolve the conflict manually." (getResolutionErrorText true res)

let isResolutionDone = function
  | Resolution.Ok _ -> true
  | _ -> false

type Resolution with
  member self.GetConflicts () = getResolutionConflicts self
  member self.GetErrorText showResolvedUpdates = getResolutionErrorText showResolvedUpdates self
  member self.GetModelOrFail () = getResolutionModelOrFail self
  member self.IsDone = isResolutionDone self

let calcOpenRequirements (exploredUpdate:ResolvedUpdate,globalMAptekaRestrictions,(versionToExplore,_),dependency,resolverStep:ResolverStep) =
  let dependenciesByName =
    // there are updates which define multiple dependencies to the same update
    // we compress these here
    let dict = Dictionary<_,_>()
    exploredUpdate.Dependencies
    |> Set.iter (fun ((name,v,r) as dep) ->
      match dict.TryGetValue name with
      | true,(_,v2,r2) ->
        match v,v2 with
        | VersionRequirement ra1, VersionRequirement ra2 ->
          let newRestrictions = MAptekaRestriction.Or [r; r2]
          if ra2 |> VersionRange.includes ra1 then
            dict.[name] <- (name,v, newRestrictions)
          elif ra1 |> VersionRange.includes ra2 then
            dict.[name] <- (name,v2,newRestrictions)
          else dict.[name] <- dep
      | _ -> dict.Add(name,dep))
    dict
    |> Seq.map (fun kv -> kv.Value)
    |> Set.ofSeq

  let rest =
    resolverStep.OpenRequirements
    |> Set.remove dependency

  dependenciesByName
  |> Set.map (fun (n, v, restriction) ->
    let newRestrictions =
      MAptekaRestriction.And [restriction; exploredUpdate.MAptekaRestriction; globalMAptekaRestrictions]
      |> fun xs -> if xs = MAptekaRestriction.noRestriction then exploredUpdate.MAptekaRestriction else xs

    { dependency
      with
        Name = n
        VersionRequirement = v
        Graph = Set.add dependency dependency.Graph
    })
  |> Set.filter (fun ({VersionRequirement = (VersionRequirement range)} as d) ->
    resolverStep.ClosedRequirements
    |> Set.exists (fun ({VersionRequirement = (VersionRequirement xrange)} as x) ->
      x.Name = d.Name &&
        (x = d ||
         range |> VersionRange.includes xrange))
    |> not)
  |> Set.filter (fun d ->
    resolverStep.OpenRequirements
    |> Set.exists (fun x -> x.Name = d.Name && x = d && x.MAptekaRestriction = d.MAptekaRestriction)
    |> not)
  |> Set.union rest

let getResolverStrategy
  globalStrategyForDirectDependencies
  globalStrategyForTransitives
  (allRequirementsOfCurrentUpdate:Set<UpdateRequirement>)
  (currentRequirement:UpdateRequirement) =
  
  if currentRequirement.Parent.IsRootRequirement() && Set.count allRequirementsOfCurrentUpdate = 1 then
    let combined = currentRequirement.ResolverStrategyForDirectDependencies ++ globalStrategyForDirectDependencies
    defaultArg combined ResolverStrategy.Max
  else
    let combined =
      (allRequirementsOfCurrentUpdate
        |> Seq.filter (fun x -> x.Depth > 0)
        |> Seq.sortBy (fun x -> x.Depth, x.ResolverStrategyForTransitives <> globalStrategyForTransitives, x.ResolverStrategyForTransitives <> Some ResolverStrategy.Max)
        |> Seq.map (fun x -> x.ResolverStrategyForTransitives)
        |> Seq.fold (++) None)
        ++ globalStrategyForTransitives

    defaultArg combined ResolverStrategy.Max

type UpdateMode =
  | UpdateFunctional of FunctionalName
  | UpdateFiltered of FunctionalName * UpdateFilter
  | Install
  | UpdateAll

type private UpdateConfig =
  { Dependency         : UpdateRequirement
    FunctionalName     : FunctionalName
    MAptekaRestriction : MAptekaRestriction
    RootSettings       : IDictionary<UpdateName,MAptekaRestriction>
    Version            : VersionInfo
    UpdateMode         : UpdateMode
  } 
  member self.HasMAptekaRestriction =
    self.MAptekaRestriction <> MAptekaRestriction.noRestriction

  member self.HasDependencyRestrictions =
    self.Dependency.MAptekaRestriction <> MAptekaRestriction.noRestriction

let private updateRestrictions (updCongig:UpdateConfig) (update:ResolvedUpdate) =
  let newRestrictions =
    if  not updCongig.HasMAptekaRestriction
      && (MAptekaRestriction.noRestriction = update.MAptekaRestriction
      ||  not updCongig.HasDependencyRestrictions )
    then
      MAptekaRestriction.noRestriction
    else
      // Restriction in the protocol file
      let globalUpdateRestriction =
        match updCongig.RootSettings.TryGetValue update.Name with
        | true, s -> s
        | _ -> MAptekaRestriction.noRestriction
      
      let updateRestriction = update.MAptekaRestriction
      let dependencyRestriction = updCongig.Dependency.MAptekaRestriction
      let globalSettings = updCongig.MAptekaRestriction
      let isRequired =
        MAptekaRestriction.Or
          [ updateRestriction
            MAptekaRestriction.And [dependencyRestriction;globalSettings]]

      // We assume the user knows what he is doing
      MAptekaRestriction.And [ globalSettings;isRequired ]


  { update with
      MAptekaRestriction = newRestrictions
  }

let private exploreUpdateConfig
  getPackageDetailsBlock
  (updConfig:UpdateConfig) =
  
  let dependency, version = updConfig.Dependency, updConfig.Version
  match updConfig.UpdateMode with
  | Install -> printf  " - %O %A" dependency.Name version
  | _ ->
    match dependency.VersionRequirement with
    | VersionRequirement (Specific _) when dependency.Parent.IsRootRequirement() ->
      printf " - %O is pinned to %O" dependency.Name version
    | _ -> printf  " - %O %A" dependency.Name version

  let newRestrictions =
    MAptekaRestriction.And [dependency.MAptekaRestriction; updConfig.MAptekaRestriction]
  try
    let updateDetails : UpdateDetails =
      getPackageDetailsBlock updConfig.FunctionalName dependency.Name version
    let filteredDependencies =
      DependencySetFilter.filterByRestrictions newRestrictions updateDetails.DirectDependencies
    Some
      {   Name                = updateDetails.Name
          Version             = version
          Dependencies        = filteredDependencies
          Unlisted            = updateDetails.Unlisted
          MAptekaRestriction  = newRestrictions
      }
  with exn ->
    printf "    Package not available.%s      Message: %s" Environment.NewLine exn.Message
    None
   
type StackPack =
  { ExploredUpdates     : Dictionary<UpdateName*VersionInfo,ResolvedUpdate>
    KnownConflicts      : HashSet<HashSet<UpdateRequirement> * (VersionInfo list * bool) option>
    ConflictHistory     : Dictionary<UpdateName, int>
  }

let private getExploredUpdate (updConfig:UpdateConfig) getUpdateDetailsBlock (stackpack:StackPack) =
  let key = (updConfig.Dependency.Name, updConfig.Version)

  match stackpack.ExploredUpdates.TryGetValue key with
  | true, update ->
    let package = updateRestrictions updConfig update
    stackpack.ExploredUpdates.[key] <- update
    printf "   Retrieved Explored Package  %O" update
    stackpack, Some(true, update)
  | false,_ ->
    match exploreUpdateConfig getUpdateDetailsBlock updConfig with
    | Some explored ->
      printf "   Found Explored Package  %O" explored
      stackpack.ExploredUpdates.Add(key,explored)
      stackpack, Some(false, explored)
    | None ->
      stackpack, None

let private getCompatibleVersions
  (currentStep:ResolverStep)
  functionalName
  (currentRequirement:UpdateRequirement)
  (getVersionsF: ResolverStrategy -> FunctionalName -> UpdateName -> VersionInfo seq)
  globalOverride
  globalStrategyForDirectDependencies
  globalStrategyForTransitives =
    
  printfn "  Trying to resolve %O" currentRequirement

  match Map.tryFind currentRequirement.Name currentStep.FilteredVersions with
  | None ->
    let allRequirementsOfCurrentPackage =
      currentStep.OpenRequirements
      |> Set.filter (fun r -> currentRequirement.Name = r.Name)

    // we didn't select a version yet so all versions are possible
    let isInRange mapF ver =
      allRequirementsOfCurrentPackage
      |> Set.forall (fun r -> (mapF r).VersionRequirement |> VersionRequirement.isInRange ver)

    let getSingleVersion v = Seq.singleton v
      
    let availableVersions =
      match currentRequirement.VersionRequirement with
      //| OverrideAll v -> getSingleVersion v
      | VersionRequirement (Specific v) -> getSingleVersion v
      | _ ->
        let resolverStrategy = getResolverStrategy globalStrategyForDirectDependencies globalStrategyForTransitives allRequirementsOfCurrentPackage currentRequirement
        getVersionsF resolverStrategy functionalName currentRequirement.Name

    Seq.filter (isInRange id) (availableVersions) |> Seq.cache

  | Some versions ->
    // we already selected a version so we can't pick a different
    versions |> Seq.filter (fun vi -> VersionRequirement.isInRange vi currentRequirement.VersionRequirement)

let private getConflicts
  (currentStep:ResolverStep)
  (currentRequirement:UpdateRequirement)
  (knownConflicts:HashSet<HashSet<UpdateRequirement> * (VersionInfo list * bool) option>) =
    
  let allRequirements =
    currentStep.OpenRequirements
    |> Set.filter (fun r -> r.Graph |> Set.contains currentRequirement |> not)
    |> Set.union currentStep.ClosedRequirements

  knownConflicts
  |> Seq.map (fun (conflicts,selectedVersion) ->
    let isSubset = conflicts.IsSubsetOf allRequirements
    match selectedVersion with
    | None when isSubset -> conflicts
    | Some(selectedVersion,_) ->
      let n = (Seq.head conflicts).Name
      match currentStep.FilteredVersions |> Map.tryFind n with
      | Some v when v = selectedVersion && isSubset -> conflicts
      | _ -> HashSet()
    | _ -> HashSet())
  |> Seq.collect id
  |> HashSet

let private getCurrentRequirement
  updateFilter 
  (openRequirements:Set<UpdateRequirement>)
  (conflictHistory:Dictionary<_,_>) =

  let initialMin = Seq.head openRequirements
  let initialBoost = 0

  let currentMin, _ =
    ((initialMin,initialBoost),openRequirements)
    ||> Seq.fold (fun (cmin,cboost) d ->
      let boost =
        match conflictHistory.TryGetValue d.Name with
        | true,c -> -c
        | _ -> 0
      if UpdateRequirement.Compare(d,cmin,updateFilter,boost,cboost) = -1 then
        d, boost
      else
        cmin,cboost)
  currentMin

type ConflictState =
  { Status               : Resolution
    LastConflictReported : DateTime
    TryRelaxed           : bool
    Conflicts            : Set<UpdateRequirement>
    VersionsToExplore    : VersionInfo seq
    //GlobalOverride       : bool
  }

let private boostConflicts
  (filteredVersions:Map<UpdateName, (VersionInfo list * bool)>)
  (currentRequirement:UpdateRequirement)
  (stackpack:StackPack)
  (conflictState:ConflictState) =

  let conflictStatus = conflictState.Status
  let isNewConflict  =
    match stackpack.ConflictHistory.TryGetValue currentRequirement.Name with
    | true,count ->
      stackpack.ConflictHistory.[currentRequirement.Name] <- count + 1
      false
    | _ ->
      stackpack.ConflictHistory.Add(currentRequirement.Name, 1)
      true

  let conflicts = conflictStatus.GetConflicts()
  let lastConflictReported =
    match conflicts with
    | _ when not conflicts.IsEmpty  ->
      let c = conflicts |> Seq.minBy (fun c -> c.Parent)
      let selectedVersion = Map.tryFind c.Name filteredVersions
      let key = conflicts |> HashSet,selectedVersion
      stackpack.KnownConflicts.Add key |> ignore

      let reportThatResolverIsTakingLongerThanExpected =
        not isNewConflict && DateTime.Now - conflictState.LastConflictReported > TimeSpan.FromSeconds 10.

      printfn "%s" <| conflictStatus.GetErrorText(false)
      printfn "    ==> Trying different resolution."
      if reportThatResolverIsTakingLongerThanExpected then
        printfn "%s" <| conflictStatus.GetErrorText(false)
        printfn "The process is taking longer than expected."
        printfn "Paket may still find a valid resolution, but this might take a while."
        DateTime.Now
      else
        conflictState.LastConflictReported
    | _ -> conflictState.LastConflictReported
  { conflictState with
      LastConflictReported = lastConflictReported }, stackpack
