module MAptekaGet.Domain

open System
open System.Text.RegularExpressions
open MAptekaGet.Utils

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
 
[<CustomEquality; CustomComparison>]
type VersionInfo = 
  { Major : uint32
    Minor : uint32
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

module VersionInfo =
  let parse (version : string) = 
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

  let zero = parse "0.0.0"
  let sortVersions =
    Array.choose (fun v -> try Some(v,parse v) with | _ -> None)
    >> Array.sortBy snd
    >> Array.map fst
    >> Array.rev

type VersionRangeBound = Excluding | Including

type VersionRange =
  | Minimum of VersionInfo
  | GreaterThan of VersionInfo
  | Maximum of VersionInfo
  | LessThan of VersionInfo
  | Specific of VersionInfo
  //| OverrideAll of VersionInfo
  | Range of fromB : VersionRangeBound * from : VersionInfo * _to : VersionInfo * _toB : VersionRangeBound

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

module VersionRange =
  let atLeast = Minimum << VersionInfo.parse
  let atMost = Maximum << VersionInfo.parse
  let exactly = Specific << VersionInfo.parse
  let between (version1, version2) = Range(VersionRangeBound.Including, VersionInfo.parse version1, VersionInfo.parse version2,VersionRangeBound.Excluding)
  // static member BasicOperators = ["~>";"==";"<=";">=";"=";">";"<"]
  // static member StrategyOperators = ['!';'@']
  let includes (other : VersionRange) (self : VersionRange) =
    match self, other with
    | Minimum v1, Minimum v2 when v1 <= v2 -> true
    | Minimum v1, Specific v2 when v1 <= v2 -> true
    | Specific v1, Specific v2 when v1 = v2 -> true
    | Range(_, min1, max1, _), Specific v2 when min1 <= v2 && max1 >= v2 -> true
    | GreaterThan v1, GreaterThan v2 when v1 < v2 -> true
    | GreaterThan v1, Specific v2 when v1 < v2 -> true
    | _ -> false

type MAptekaRestriction =
  | Exactly of VersionInfo
  | AtLeast of VersionInfo
  | Not     of MAptekaRestriction
  | Or      of MAptekaRestriction list
  | And     of MAptekaRestriction list

  override this.ToString() =
    match this with
    | MAptekaRestriction.Exactly r -> "== " + r.ToString()
    | MAptekaRestriction.AtLeast r -> ">= " + r.ToString()
    | MAptekaRestriction.Not(MAptekaRestriction.AtLeast r) -> sprintf "< " + r.ToString()
    | MAptekaRestriction.Not(fr) -> sprintf "NOT (%O)" fr
    | MAptekaRestriction.Or(frl) ->
      match frl with
      | [] -> "false"
      | [single] -> sprintf "%O" single
      | _ -> sprintf "|| %s" (System.String.Join(" ", frl |> Seq.map (sprintf "(%O)")))
    | MAptekaRestriction.And(frl) ->
      match frl with
      | [] -> "true"
      | [single] -> sprintf "%O" single
      | _ -> sprintf "&& %s" (System.String.Join(" ", frl |> Seq.map (sprintf "(%O)")))

module MAptekaRestriction =
  let rec isMatch (vi : VersionInfo) (mar : MAptekaRestriction) =
    match mar with
    | Exactly vi1 -> vi1 = vi
    | AtLeast vi1 -> Minimum vi1 |> VersionRange.includes (Minimum vi)
    | Not r       -> r |> isMatch vi |> not
    | Or rs       -> rs |> List.exists (isMatch vi)
    | And rs      -> rs |> List.forall (isMatch vi)
  
  let noRestriction = AtLeast VersionInfo.zero
  let rec isSubsetOf (y : MAptekaRestriction) (x : MAptekaRestriction) =
    let includes x y = x |> isSubsetOf y
    match x with
    | Exactly x' ->
      match y with
      | Exactly y' -> x' = y'
      | AtLeast y' -> y' <= x'
      | Not (AtLeast y') -> y' > x'
      | Not (Exactly y') -> x' <> y'
      | Not y' -> y' |> isMatch x' |> not
      | Or ys -> ys |> List.exists (includes x)
      | And ys -> ys |> List.forall (includes x)
    | AtLeast x' ->
      match y with
      | Exactly _ -> false
      | AtLeast y' -> y' <= x'
      | Not (AtLeast y') -> false
      | Not (Exactly y') -> x |> isMatch y' |> not
      | Not y' -> not (y' |> isMatch x') && not (y' |> isSubsetOf x) 
      | Or ys -> ys |> List.exists (includes x)
      | And ys -> ys |> Seq.forall (includes x)
    
    | Not (AtLeast x' as notX) ->
      match y with
      | Exactly _ | AtLeast _ -> false 
      | Not (AtLeast y' as notY) -> notY |> isSubsetOf notX
      | Not (Exactly y' as notY) -> notY |> isSubsetOf notX
      | Not y' -> y' |> isSubsetOf notX
      | Or ys -> ys |> List.exists (includes x)
      | And ys -> ys |> List.forall (includes x)
    | Not (Exactly x' as notX) ->
      match y with
      | Exactly _ | AtLeast _ -> false 
      | Not (AtLeast y' as notY) -> notY |> isSubsetOf notX
      | Not (Exactly y' as notY) -> notY |> isSubsetOf notX
      | Not y' -> y' = Exactly x'
      | Or ys -> ys |> List.exists (includes x)
      | And ys -> ys |> List.forall (includes x)
    
    | Not (Not x') -> x' |> isSubsetOf y
    | Not (Or xs) -> (And (xs |> List.map Not)) |> isSubsetOf y
    | Not (And xs) -> (Or (xs |> List.map Not)) |> isSubsetOf y

    | Or xs -> xs |> List.forall (fun xi -> xi |> isSubsetOf y)
    | And xs -> xs |> List.exists (fun xi -> xi |> isSubsetOf y)

type VersionRequirement =
  | VersionRequirement of VersionRange

  override this.ToString() =
    let (VersionRequirement range) = this in range.ToString()

module VersionRequirement =
  /// Checks wether the given version is in the version range
  let isInRange (version : VersionInfo) (VersionRequirement range) =
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

  let range (VersionRequirement range) = range

  let noRestriction = VersionRequirement(Minimum(VersionInfo.zero))

  let parse text =
    if String.IsNullOrWhiteSpace text || text = "null" then noRestriction else

    let analyzeVersion operator (text:string) =
      try
        if text.Contains "*" then
          VersionRange.Minimum (VersionInfo.parse (text.Replace("*","0")))
        else
          operator (VersionInfo.parse text)
      with
      | exn -> failwithf "Error while parsing %s%sMessage: %s" text Environment.NewLine exn.Message

    let analyzeVersionSimple (text:string) =
      try
        VersionInfo.parse (text.Replace("*","0"))
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
            | 0 -> Minimum(VersionInfo.zero)
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
    MAptekaRestriction : MAptekaRestriction
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

type DependencySet = Set<UpdateName * VersionRequirement * MAptekaRestriction>




