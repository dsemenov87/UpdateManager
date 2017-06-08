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
    Dependencies        : DependencySet
    Unlisted            : bool
  } 
  override this.ToString () = sprintf "%O %O" this.Name this.Version

type UpdateResolution = Map<UpdateName, ResolvedUpdate>

module DependencySetFilter =
  let isIncluded (restriction:VersionRequirement) (dependency:UpdateName * VersionRequirement * VersionRequirement) =
    match dependency with
    | _, _, vr when vr = VersionRequirement.NoRestriction -> true
    | _, _, (VersionRequirement vr) ->
      let (VersionRequirement vr1) = restriction
      vr.IsIncludedIn vr1

  let filterByRestrictions (restriction:VersionRequirement) (dependencies:DependencySet) : DependencySet =
    if restriction = VersionRequirement.NoRestriction then
      dependencies
    else
      dependencies
      |> Set.filter (isIncluded restriction)

  let isUpdateCompatible (dependencies:DependencySet) (update:ResolvedUpdate) : bool =
    dependencies
    // exists any not matching stuff
    |> Seq.exists (fun (name, requirement, restriction) ->
      if name = update.Name && not (requirement.IsInRange update.Version) then
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
    FilteredVersions : Map<UpdateName, (VersionInfo list * bool)>
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
    // there are packages which define multiple dependencies to the same package
    // we compress these here
    let dict = Dictionary<_,_>()
    exploredUpdate.Dependencies
    |> Set.iter (fun ((name,v,r) as dep) ->
      match dict.TryGetValue name with
      | true,(_,v2,r2) ->
        match v,v2 with
        | VersionRequirement ra1, VersionRequirement ra2 ->
          // let newRestrictions =
          //   match r with
          //   | Exactly pr ->
          //     match r2 with
          //     | Exactly pr2 ->
          //       MAptekaRestriction.combineRestrictionsWithOr r r2 |> ExplicitRestriction
          //     | AutoDetectFramework -> ExplicitRestriction r
          //   | AutoDetectFramework -> r

          if ra1.IsIncludedIn ra2 then
            dict.[name] <- (name,v, r.MergeWith r2)
          elif ra2.IsIncludedIn ra1 then
            dict.[name] <- (name,v2,r.MergeWith r2)
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
    // let newRestrictions =
    //   filterRestrictions restriction exploredPackage.Settings.FrameworkRestrictions
    //   |> filterRestrictions globalFrameworkRestrictions
    //   |> fun xs -> if xs = ExplicitRestriction FrameworkRestriction.NoRestriction then exploredPackage.Settings.FrameworkRestrictions else xs

    { dependency
      with
        Name = n
        VersionRequirement = v
        Graph = Set.add dependency dependency.Graph
    })
  |> Set.filter (fun d ->
      resolverStep.ClosedRequirements
      |> Set.exists (fun x ->
          x.Name = d.Name &&
             //x.Settings.FrameworkRestrictions = d.Settings.FrameworkRestrictions &&
              (x = d ||
               x.VersionRequirement.Range.IsIncludedIn d.VersionRequirement.Range))
      |> not)
  |> Set.filter (fun d ->
      resolverStep.OpenRequirements
      |> Set.exists (fun x -> x.Name = d.Name && (x = d)) //&& x.Settings.FrameworkRestrictions = d.Settings.FrameworkRestrictions)
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
    GlobalRestrictions : VersionRequirement
    RootSettings       : IDictionary<UpdateName,VersionRequirement>
    Version            : VersionInfo
    UpdateMode         : UpdateMode
  } 
  member self.HasGlobalRestrictions =
    self.GlobalRestrictions <> VersionRequirement.NoRestriction

  member self.HasDependencyRestrictions =
      self.Dependency.MAptekaRequirement <> VersionRequirement.NoRestriction

let private updateRestrictions (pkgConfig:PackageConfig) (package:ResolvedPackage) =
    let newRestrictions =
        if  not pkgConfig.HasGlobalRestrictions
            && (FrameworkRestriction.NoRestriction = (package.Settings.FrameworkRestrictions |> getExplicitRestriction)
            ||  not pkgConfig.HasDependencyRestrictions )
        then
            FrameworkRestriction.NoRestriction
        else
            // Setting in the dependencies file
            let globalPackageSettings =
                match pkgConfig.RootSettings.TryGetValue package.Name with
                | true, s -> 
                    match s.FrameworkRestrictions with
                    | ExplicitRestriction r -> r
                    | _ -> FrameworkRestriction.NoRestriction
                | _ -> FrameworkRestriction.NoRestriction
            // Settings required for the current resolution
            let packageSettings = package.Settings.FrameworkRestrictions |> getExplicitRestriction
            // Settings required for this current dependency
            let dependencySettings = pkgConfig.Dependency.Settings.FrameworkRestrictions |> getExplicitRestriction
            // Settings defined globally
            let globalSettings = pkgConfig.GlobalRestrictions |> getExplicitRestriction
            let isRequired =
                FrameworkRestriction.Or
                  [ packageSettings
                    FrameworkRestriction.And [dependencySettings;globalSettings]]

            // We assume the user knows what he is doing
            FrameworkRestriction.And [ globalPackageSettings;isRequired ]


    { package with
        Settings = { package.Settings with FrameworkRestrictions = ExplicitRestriction newRestrictions }
    }
