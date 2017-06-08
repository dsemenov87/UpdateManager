module DependencyResolution
  open System.Collections.Generic
  open MAptekaGet.Domain
  open MAptekaGet.Domain.Dependencies
  open MAptekaGet.Domain.Versioning
  open MAptekaGet.Domain.Requirements

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
    } with
    override this.ToString () = sprintf "%O %O" this.Name this.Version
  
  type UpdateResolution = Map<UpdateName, ResolvedUpdate>

  module DependencySetFilter =
    let isIncluded (restriction:MAptekaRestriction) (dependency:UpdateName * VersionRequirement * MAptekaRestriction) =
      match dependency with
      | _,_,NoRestriction ->
        true
      | _,_,Exactly restr ->
        restriction = Exactly restr

    let filterByRestrictions (restriction:MAptekaRestriction) (dependencies:DependencySet) : DependencySet =
      match restriction with
      | NoRestriction -> dependencies
      | _ ->
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
        { upd with
            Dependencies =
                upd.Dependencies
                |> Set.map (fun (name, v, d) -> model.[name].Name, v, d) }) 

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
    | Conflict of resolveStep    : ResolverStep
                * requirementSet : UpdateRequirement Set
                * requirement    : UpdateRequirement
                * getUpdateVersions : (UpdateName -> VersionInfo seq)

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Resolution =
    open System
    open System.Text

    let getConflicts (res:Resolution) =
        match res with
        | Resolution.Ok _ -> Set.empty
        | Resolution.Conflict (currentStep,_,lastUpdateRequirement,_) ->
            currentStep.ClosedRequirements
            |> Set.union currentStep.OpenRequirements
            |> Set.add lastUpdateRequirement
            |> Set.filter (fun x -> x.Name = lastUpdateRequirement.Name)

    let buildConflictReport (errorReport:StringBuilder)  (conflicts:UpdateRequirement Set) =
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

    let getErrorText showResolvedUpdates = function
      | Resolution.Ok _ -> ""
      | Resolution.Conflict (currentStep,_,_,getVersionF) as res ->
        let errorText =
          if showResolvedUpdates && not currentStep.CurrentResolution.IsEmpty then
            ( StringBuilder().AppendLine  "  Resolved updates:"
            , currentStep.CurrentResolution)
            ||> Map.fold (fun sb _ resolvedUpdate ->
                sb.AppendLine (sprintf "   - %O %O" resolvedUpdate.Name resolvedUpdate.Version))
          else StringBuilder()

        match getConflicts res with
        | c when c.IsEmpty  ->
          errorText.AppendLine(sprintf
            "  Could not resolve update %O. Unknown resolution error."
              (Seq.head currentStep.OpenRequirements))
        | cfs when cfs.Count = 1 ->
          let c = cfs.MinimumElement
          let errorText = buildConflictReport errorText cfs
          match getVersionF c.Name |> Seq.toList with
          | [] -> errorText.AppendLine(sprintf  "   - No versions available.")
          | avalaibleVersions ->
            ( errorText.AppendLine(sprintf  "   - Available versions:")
            , avalaibleVersions )
            ||> List.fold (fun sb elem -> sb.AppendLine(sprintf "     - %O" elem))
        | conflicts -> buildConflictReport errorText conflicts
        |> string


    let getModelOrFail = function
      | Resolution.Ok model -> model
      | Resolution.Conflict _ as res ->
        failwithf  "There was a version conflict during update resolution.\n\
                    %s\n  Please try to relax some conditions or resolve the conflict manually." (getErrorText true res)

    let isDone = function
      | Resolution.Ok _ -> true
      | _ -> false

  type Resolution with
    member self.GetConflicts () = Resolution.getConflicts self
    member self.GetErrorText showResolvedUpdates = Resolution.getErrorText showResolvedUpdates self
    member self.GetModelOrFail () = Resolution.getModelOrFail self
    member self.IsDone = Resolution.isDone self

  let calcOpenRequirements (exploredUpdate:ResolvedUpdate,globalMAptekaRestrictions,(versionToExplore,_),dependency,resolverStep:ResolverStep) =
    let dependenciesByName =
        // there are packages which define multiple dependencies to the same package
        // we compress these here - see #567
        let dict = Dictionary<_,_>()
        exploredUpdate.Dependencies
        |> Set.iter (fun ((name,v,r) as dep) ->
            match dict.TryGetValue name with
            | true,(_,v2,r2) ->
                match v,v2 with
                | VersionRequirement ra1, VersionRequirement ra2 ->
                    let newRestrictions =
                        match r with
                        | ExplicitRestriction r ->
                            match r2 with
                            | ExplicitRestriction r2 ->
                                FrameworkRestriction.combineRestrictionsWithOr r r2 |> ExplicitRestriction
                            | AutoDetectFramework -> ExplicitRestriction r
                        | AutoDetectFramework -> r

                    if ra1.IsIncludedIn ra2 then
                        dict.[name] <- (name,v,newRestrictions)
                    elif ra2.IsIncludedIn ra1 then
                        dict.[name] <- (name,v2,newRestrictions)
                    else dict.[name] <- dep
                | _ ->  dict.[name] <- dep
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
            filterRestrictions restriction exploredPackage.Settings.FrameworkRestrictions
            |> filterRestrictions globalFrameworkRestrictions
            |> fun xs -> if xs = ExplicitRestriction FrameworkRestriction.NoRestriction then exploredPackage.Settings.FrameworkRestrictions else xs

        { dependency with
            Name = n
            VersionRequirement = v
            Parent = Package(dependency.Name, versionToExplore, exploredPackage.Source)
            Graph = Set.add dependency dependency.Graph
            Settings = { dependency.Settings with FrameworkRestrictions = newRestrictions } })
    |> Set.filter (fun d ->
        resolverStep.ClosedRequirements
        |> Set.exists (fun x ->
            x.Name = d.Name &&
               x.Settings.FrameworkRestrictions = d.Settings.FrameworkRestrictions &&
                (x = d ||
                 x.VersionRequirement.Range.IsIncludedIn d.VersionRequirement.Range ||
                 x.VersionRequirement.Range.IsGlobalOverride))
        |> not)
    |> Set.filter (fun d ->
        resolverStep.OpenRequirements
        |> Set.exists (fun x -> x.Name = d.Name && (x = d || x.VersionRequirement.Range.IsGlobalOverride) && x.Settings.FrameworkRestrictions = d.Settings.FrameworkRestrictions)
        |> not)
    |> Set.union rest