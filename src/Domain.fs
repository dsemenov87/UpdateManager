module MAptekaGet.Domain

open System
open System.Text.RegularExpressions
open Utils

type UpdateName =
  | UpdateName of name:string * compareString:string

type FunctionalName =
  | FunctionalName of name:string * compareString:string

type QualifiedUpdateName = 
  | QualifiedUpdateName of functional:FunctionalName * update:UpdateName

// Represents a filter of normalized update names
type UpdateFilter =
  | Name of UpdateName
  | UpdateFilter of string
  member this.Match (updateName : UpdateName) =
    match this with
    | Name name -> name = updateName
    | UpdateFilter f ->
      let regex =
        Regex("^" + f + "$",
          RegexOptions.Compiled 
          ||| RegexOptions.CultureInvariant 
          ||| RegexOptions.IgnoreCase)

      regex.IsMatch (let (UpdateName (_,cs)) : UpdateName = updateName in cs)

  static member OfName name = UpdateFilter.Name name

  override this.ToString() =
    match this with
    | Name name -> name.ToString()
    | UpdateFilter filter -> filter

type DomainMessage = 
  | DownloadError of string
  override this.ToString() = 
    match this with
    | DownloadError ip ->
      sprintf "Error occured while downloading from %s." ip
 
/// Contains the version information.
[<CustomEquality; CustomComparison>]
type VersionInfo = 
  { /// MAJOR version when you make incompatible API changes.
    Major : uint32
    /// MINOR version when you add functionality in a backwards-compatible manner.
    Minor : uint32
    /// PATCH version when you make backwards-compatible bug fixes.
    Patch : uint32
  } 
  override x.ToString() = 
    sprintf "%d.%d.%d" x.Major x.Minor x.Patch

  override x.Equals (that) = 
    match that with
    | :? VersionInfo as vi ->
      x.Major = vi.Major && x.Minor = vi.Minor && x.Patch = vi.Patch
    | _ ->
      false

  override x.GetHashCode () = hash (x.Major + x.Minor + x.Patch) 
  
  interface System.IComparable with
    member x.CompareTo yobj = 
      match yobj with
      | :? VersionInfo as y -> 
        if x.Major <> y.Major then compare x.Major y.Major
        else if x.Minor <> y.Minor then compare x.Minor y.Minor
        else compare x.Patch y.Patch

      | _ -> invalidArg "yobj" "cannot compare values of different types"


  static member Parse (version : string) = 
    try
      /// sanity check to make sure that all of the integers in the string are positive.
      /// because we use raw substrings with dashes this is very complex :(
      version.Split([|'.'|]) |> Array.iter (fun s -> match Int32.TryParse s with | true, s when s < 0 -> failwith "no negatives!" | _ -> ignore ())

      if version.Contains("!") then 
        failwithf "Invalid character found in %s" version
      if version.Contains("..") then 
        failwithf "Empty version part found in %s" version

      let firstDash = version.IndexOf("-")
      let plusIndex = version.IndexOf("+")

      let majorMinorPatch =
        let firstSigil = if firstDash > 0 then firstDash else plusIndex
        match firstSigil with
        | -1 -> version
        | n -> version.Substring(0, n)
  
      let major, minor, patch =
        match majorMinorPatch.Split([|'.'|]) with
        | [|M; m; p|] -> uint32 M, uint32 m, uint32 p
        | [|M; m;|] -> uint32 M, uint32 m, 0u
        | [|M;|] -> uint32 M, 0u, 0u
        | _ -> 0u, 0u, 0u

      { Major = major
        Minor = minor
        Patch = patch
      }

    with
    | exn ->
        failwithf "Can't parse \"%s\".%s%s" version Environment.NewLine exn.Message

  static member Zero = VersionInfo.Parse "0"
  static member SortVersions =
    Array.choose (fun v -> try Some(v,VersionInfo.Parse v) with | _ -> None)
    >> Array.sortBy snd
    >> Array.map fst
    >> Array.rev


/// Defines if the range bound is including or excluding.
[<RequireQualifiedAccess>]
type VersionRangeBound = Excluding | Including

/// Represents version information.
type VersionRange =
  | Minimum of VersionInfo
  | GreaterThan of VersionInfo
  | Maximum of VersionInfo
  | LessThan of VersionInfo
  | Specific of VersionInfo
  //| OverrideAll of VersionInfo
  | Range of fromB : VersionRangeBound * from : VersionInfo * _to : VersionInfo * _toB : VersionRangeBound

  static member AtLeast version = Minimum(VersionInfo.Parse version)
  static member AtMost version = Maximum(VersionInfo.Parse version)
  static member Exactly version = Specific(VersionInfo.Parse version)
  static member Between(version1,version2) = Range(VersionRangeBound.Including, VersionInfo.Parse version1, VersionInfo.Parse version2,VersionRangeBound.Excluding)
  static member BasicOperators = ["~>";"==";"<=";">=";"=";">";"<"]
  static member StrategyOperators = ['!';'@']
  member this.IsIncludedIn (other : VersionRange) =
    match other, this with
    | Minimum v1, Minimum v2 when v1 <= v2 -> true
    | Minimum v1, Specific v2 when v1 <= v2 -> true
    | Specific v1, Specific v2 when v1 = v2 -> true
    | Range(_, min1, max1, _), Specific v2 when min1 <= v2 && max1 >= v2 -> true
    | GreaterThan v1, GreaterThan v2 when v1 < v2 -> true
    | GreaterThan v1, Specific v2 when v1 < v2 -> true
    | _ -> false
  override this.ToString() =
    match this with
    | Specific v -> v.ToString()
    //| OverrideAll v -> "== " + v.ToString()
    | Minimum v -> ">= " + v.ToString()
    | GreaterThan v -> "> " + v.ToString()
    | Maximum v -> "<= " + v.ToString()
    | LessThan v -> "< " + v.ToString()
    | Range(fromB, from, _to, _toB) ->
      let from =
        match fromB with
        | VersionRangeBound.Excluding -> "> " + from.ToString()
        | VersionRangeBound.Including -> ">= " + from.ToString()

      let _to =
        match _toB with
        | VersionRangeBound.Excluding -> "< " + _to.ToString()
        | VersionRangeBound.Including -> "<= " + _to.ToString()

      from + " " + _to

type VersionRequirement =
  | VersionRequirement of VersionRange
  
  /// Checks wether the given version is in the version range
  member this.IsInRange(version : VersionInfo) =
    let (VersionRequirement (range)) = this
    let sameVersion v=
      v.Major = version.Major && v.Minor = version.Minor && v.Patch = version.Patch

    match range with
    | Specific v -> v = version || sameVersion v
    //| OverrideAll v -> v = version
    | Minimum v -> v = version || (v <= version) || sameVersion v
    | GreaterThan v -> v < version
    | Maximum v -> v = version || (v >= version)
    | LessThan v -> v > version && not (sameVersion v)
    | Range(fromB, from, _to, _toB) ->
      let isInUpperBound =
        match _toB with
        | VersionRangeBound.Including -> version <= _to
        | VersionRangeBound.Excluding -> version < _to && not (sameVersion _to)

      let isInLowerBound =
        match fromB with
        | VersionRangeBound.Including -> version >= from
        | VersionRangeBound.Excluding -> version > from

      (isInLowerBound && isInUpperBound) || sameVersion from

  member this.Range =
    match this with
    | VersionRequirement(range) -> range

  static member NoRestriction = VersionRequirement(Minimum(VersionInfo.Zero))
  override this.ToString() = this.Range.ToString()

  member this.IntersectWith (other : VersionRequirement) : VersionRequirement =
    //todo
    failwith "Not implemented"
  static member Parse text =
    if String.IsNullOrWhiteSpace text || text = "null" then VersionRequirement.NoRestriction else

    let analyzeVersion operator (text:string) =
      try
        if text.Contains "*" then
          VersionRange.Minimum (VersionInfo.Parse (text.Replace("*","0")))
        else
          operator (VersionInfo.Parse text)
      with
      | exn -> failwithf "Error while parsing %s%sMessage: %s" text Environment.NewLine exn.Message

    let analyzeVersionSimple (text:string) =
      try
        VersionInfo.Parse (text.Replace("*","0"))
      with
      | exn -> failwithf "Error while parsing %s%sMessage: %s" text Environment.NewLine exn.Message

    let parseRange (text:string) =

        let parseBound s = 
          match s with
          | '[' | ']' -> VersionRangeBound.Including
          | '(' | ')' -> VersionRangeBound.Excluding
          | _         -> failwithf "unable to parse bound %O in %s" s text

        let parsed =
          if not <| text.Contains "," then
            if text.StartsWith "[" then 
              text.Trim([|'['; ']'|]) 
              |> analyzeVersion Specific
            else analyzeVersion Minimum text
          else
            let fromB = parseBound text.[0]
            let toB   = parseBound (Seq.last text)
            let versions =
              text
                .Trim([|'['; ']';'(';')'|])
                .Split([|','|], StringSplitOptions.RemoveEmptyEntries)
                |> Array.filter (String.IsNullOrWhiteSpace >> not)
                |> Array.map analyzeVersionSimple

            match versions.Length with
            | 2 ->
                Range(fromB, versions.[0], versions.[1], toB)
            | 1 ->
              if text.[1] = ',' then
                match fromB, toB with
                | VersionRangeBound.Excluding, VersionRangeBound.Including -> Maximum(versions.[0])
                | VersionRangeBound.Excluding, VersionRangeBound.Excluding -> LessThan(versions.[0])
                | VersionRangeBound.Including, VersionRangeBound.Including -> Maximum(versions.[0])
                | _ -> failwithf "unable to parse %s" text
              else
                match fromB, toB with
                | VersionRangeBound.Excluding, VersionRangeBound.Excluding -> GreaterThan(versions.[0])
                | VersionRangeBound.Including, VersionRangeBound.Including -> Minimum(versions.[0])
                | VersionRangeBound.Including, VersionRangeBound.Excluding -> Minimum(versions.[0])
                | _ -> failwithf "unable to parse %s" text
            | 0 -> Minimum(VersionInfo.Zero)
            | _ -> failwithf "unable to parse %s" text
        match parsed with
        | Range(fromB, from, _to, _toB) -> 
          if (fromB = VersionRangeBound.Including) && (_toB = VersionRangeBound.Including) && (from = _to) then
            Specific from
          else
            parsed
        | x -> x
    let range = parseRange text

    VersionRequirement(range)

/// Represents a resolver strategy.
[<RequireQualifiedAccess>]
type ResolverStrategy = Max | Min

type VersionStrategy =
  { VersionRequirement : VersionRequirement
    ResolverStrategy : ResolverStrategy option
  }

type UpdateRequirementSource =
  | ProtocolFile of string
  | Update of UpdateName * VersionInfo
    member this.IsRootRequirement() =
      match this with
      | ProtocolFile _  -> true
      | _               -> false

    override this.ToString() =
      match this with
      | ProtocolFile x        -> x
      | Update(name,version)  -> sprintf "%O %O" name version

/// Represents an unresolved update.
[<CustomEquality;CustomComparison>]
type UpdateRequirement =
  { Name : UpdateName
    VersionRequirement : VersionRequirement
    MAptekaRequirement : VersionRequirement
    ResolverStrategyForDirectDependencies : ResolverStrategy option
    ResolverStrategyForTransitives : ResolverStrategy option
    Parent: UpdateRequirementSource
    Graph: UpdateRequirement Set
  }

  override this.Equals(that) = 
    match that with
    | :? UpdateRequirement as that -> 
      this.Name = that.Name && 
      this.VersionRequirement = that.VersionRequirement && 
      this.ResolverStrategyForTransitives = that.ResolverStrategyForTransitives && 
      this.ResolverStrategyForDirectDependencies = that.ResolverStrategyForDirectDependencies &&
      this.Parent = that.Parent
    | _ -> false

  override this.ToString() =
      sprintf "%O %O" this.Name this.VersionRequirement

  override this.GetHashCode() = hash (this.Name,this.VersionRequirement)
  
  member this.Depth = this.Graph.Count

  static member Compare(x : UpdateRequirement, y: UpdateRequirement, startWithUpdate:UpdateFilter option,boostX,boostY) =
    if obj.ReferenceEquals(x, y) then 0 else
    let c = compare x.Depth y.Depth
    if c <> 0 then c else
    let c = match startWithUpdate with
                | Some filter when filter.Match x.Name -> -1
                | Some filter when filter.Match y.Name -> 1
                | _ -> 0
    if c <> 0 then c else
    let c = -compare x.ResolverStrategyForDirectDependencies y.ResolverStrategyForDirectDependencies
    if c <> 0 then c else
    let c = -compare x.ResolverStrategyForTransitives y.ResolverStrategyForTransitives
    if c <> 0 then c else
    let c = compare boostX boostY
    if c <> 0 then c else
    let c = -compare x.VersionRequirement y.VersionRequirement
    if c <> 0 then c else
    let c = compare x.Parent y.Parent
    if c <> 0 then c else
    let c = compare x.Name y.Name
    if c <> 0 then c else 0

  interface System.IComparable with
    member this.CompareTo that = 
      match that with 
      | :? UpdateRequirement as that -> UpdateRequirement.Compare(this,that,None,0,0)
      | _ -> invalidArg "that" "cannot compare value of different types"

type InstallOptions = 
  { Strict : bool 
    Redirects : bool option
    ResolverStrategyForDirectDependencies : ResolverStrategy option
    ResolverStrategyForTransitives : ResolverStrategy option
    //Settings : InstallSettings
  }
  static member Default =
    { Strict = false
      Redirects = None
      ResolverStrategyForTransitives = None
      ResolverStrategyForDirectDependencies = None
      //Settings = InstallSettings.Default
    }

type Functional =
  { Name: FunctionalName
    Options: InstallOptions
    Updates : UpdateRequirement list
  }
  static member New(name) =
    { Name = name
      Options = InstallOptions.Default
      Updates = []
    }

  member this.CombineWith (other:Functional) =
    { Name = this.Name
      Options = 
        { Redirects = this.Options.Redirects ++ other.Options.Redirects
          Strict = this.Options.Strict || other.Options.Strict
          ResolverStrategyForDirectDependencies = this.Options.ResolverStrategyForDirectDependencies ++ other.Options.ResolverStrategyForDirectDependencies 
          ResolverStrategyForTransitives = this.Options.ResolverStrategyForTransitives ++ other.Options.ResolverStrategyForTransitives 
        }
      
      Updates = this.Updates @ other.Updates  
    }

type DependencySet = Set<UpdateName * VersionRequirement * VersionRequirement> // name, upd version, mapteka version




