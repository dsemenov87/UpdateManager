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

    with ex ->
      failwithf "Can't parse \"%s\".%s%s" version Environment.NewLine ex.Message

  let zero = { Major = 0u; Minor = 0u; Patch = 0u }

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
        | Excluding -> "> " + from.ToString()
        | Including -> ">= " + from.ToString()

      let _to =
        match _toB with
        | Excluding -> "< " + _to.ToString()
        | Including -> "<= " + _to.ToString()

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
    | Range(lb, min1, max1, rb), Specific v2 ->
      let leftValid = match lb with Excluding -> min1 < v2 | Including -> min1 <= v2
      let rightValid = match rb with Excluding -> max1 > v2 | Including -> max1 >= v2
      in leftValid && rightValid

    | GreaterThan v1, GreaterThan v2 when v1 < v2 -> true
    | GreaterThan v1, Specific v2 when v1 < v2 -> true
    | Maximum v1, Maximum v2 when v1 >= v2 -> true
    | Maximum v1, Specific v2 when v1 >= v2 -> true
    | LessThan v1, LessThan v2 when v1 > v2 -> true
    | LessThan v1, Specific v2 when v1 > v2 -> true
    | _ -> false

type MAptekaRestriction =
  | Exactly of VersionInfo
  | AtLeast of VersionInfo
  | Not     of MAptekaRestriction
  | Or      of MAptekaRestriction list
  | And     of MAptekaRestriction list

  override this.ToString() =
    match this with
    | Exactly r -> "== " + r.ToString()
    | AtLeast r -> ">= " + r.ToString()
    | Not (AtLeast r) -> sprintf "< " + r.ToString()
    | Not fr -> sprintf "NOT (%O)" fr
    | Or rl ->
      match rl with
      | [] -> "false"
      | [single] -> sprintf "%O" single
      | _ -> sprintf "|| %s" (System.String.Join(" ", rl |> Seq.map (sprintf "(%O)")))
    | And rl ->
      match rl with
      | [] -> "true"
      | [single] -> sprintf "%O" single
      | _ -> sprintf "&& %s" (System.String.Join(" ", rl |> Seq.map (sprintf "(%O)")))

module MAptekaRestriction =
  let noRestriction = AtLeast VersionInfo.zero
  
  /// translate to "normalized" form: (restr_1 && restr_2) || .. (restr_n-1 && restr_n)
  let internal normalize (mr: MAptekaRestriction) : MAptekaRestriction =
    let rec calc mr =
      match mr with
      | Or rs   -> rs |> List.collect calc
      | And rs  ->
        rs
        |> List.fold (fun acc r ->
          let ns = calc r
          match acc with
          | []  -> ns
          | _   -> ns |> List.map (fun n -> And (n :: acc))
        ) [] 
      | Not (Or rs) ->
        calc (rs |> List.map Not |> And)

      | Not (And rs) ->
        calc (rs |> List.map Not |> Or)

      | _ -> [mr]
      
    calc mr |> Or

  /// When we have a restriction like (>=3.5.0 && <4.5.0) || >=4.5.0
  /// then we can "optimize" / simplify to (>=3.5.0 || >=4.5.0)
  /// because we don't need to "pseudo" restrict the set with the first restriction 
  /// when we add back later all the things we removed.
  /// Generally: We can remove all negated literals in all clauses when a positive literal exists as a standalone Or clause
  let internal removeNegatedLiteralsWhichOccurSinglePositive (mr: MAptekaRestriction) =
    match mr with
    | Or orClauses ->
      let positiveSingles = orClauses |> List.choose (function AtLeast h -> Some h | _ -> None)
      orClauses
      |> List.fold (fun (formulas: MAptekaRestriction list) (orFormula: MAptekaRestriction) ->
        match orFormula with
        | Not (AtLeast vi) ->
          if positiveSingles |> List.exists ((=) vi) then
            formulas
          else
            orFormula :: formulas
        
        | And rs_ ->
          rs_
          |> List.choose (fun mr ->
            match mr with 
            | Not (AtLeast vi) ->
              if positiveSingles |> List.exists ((=) vi) |> not then None else Some mr
            | _ ->
              Some mr
          )
          |> List.append formulas

        | formula -> formula :: formulas
      ) []
      |> Or
    
    | _ -> mr
      
  /// ((>=2.0.0) && (>=4.0.0)) || (>=2.0.0) can be simplified to (>=2.0.0) because any AND clause with (>=2.0.0) can be removed.
  let internal removeUneccessaryOrClauses (mr: MAptekaRestriction) =
    match mr with
    | Or orClauses ->
      let isContained (andList: MAptekaRestriction list) (item: MAptekaRestriction) =
        match item with
        | And andClauses ->
          if andClauses.Length >= andList.Length then
            false
          else
            andClauses |> List.forall (fun lit -> andList |> List.contains lit)
        
        | _ ->
          andList |> List.contains item

      orClauses
      |> List.filter (fun orClause ->
        orClauses
        |> List.exists (function
          | And andClauses ->
            andClauses |> List.contains orClause
          | _ ->
            
        )
        |> not
      )
      |> Or

    | _ -> mr

  /// clauses with ((>=2.0.0) && (<2.0.0) && ...) can be removed because they contains a literal and its negation.
  let internal removeUneccessaryAndClauses (mr: MAptekaRestriction) =
    match mr with
    | Or orClauses ->
      orClauses
      |> List.map (function
        | And andClauses  -> andClauses
        | r               -> [r] 
      )
      |> List.filter (fun andList ->
        let normalizeLiterals =
          andList
          |> List.map (function Exactly vi -> AtLeast vi | Not (Exactly vi) -> Not (AtLeast vi) | mr -> mr)
        normalizeLiterals
        |> List.exists (fun r -> normalizeLiterals |> List.contains r)
        |> not
      )
      |> List.map And
      |> Or

    | _ -> mr

  /// When we optmized a clause away completely we can replace the hole formula with "NoRestriction"
  /// This happens for example with ( <4.5.0 || >=4.5.0) and the removeNegatedLiteralsWhichOccurSinglePositive
  /// optimization
  let internal replaceWithNoRestrictionIfAnyLiteralListIsEmpty (mr: MAptekaRestriction) =
    match mr with
    | Or []         -> noRestriction
    | Or orClauses  ->
      orClauses
      |> List.exists (function And [] -> true | _ -> false)
      |> function true -> noRestriction | _ -> Or orClauses

    | _ -> mr
  let internal simplify (mr: MAptekaRestriction) =
    let sortClauses (mr: MAptekaRestriction) =
      match mr with
      | Or orClauses ->
        orClauses
        |> List.map (function And andClauses -> andClauses |> List.distinct |> List.sort |> And | r -> r)
        |> List.distinct
        |> List.sort 
        |> Or
      | _ ->
        mr

    let optimize =
      normalize
      >> removeNegatedLiteralsWhichOccurSinglePositive
      >> removeUneccessaryAndClauses
      >> removeUneccessaryOrClauses
      >> replaceWithNoRestrictionIfAnyLiteralListIsEmpty
      >> sortClauses

    let rec loop formula =
      let newFormula = optimize formula
      if newFormula = formula then
        formula
      else
        loop newFormula

    loop mr

  let and' (left : MAptekaRestriction) (right : MAptekaRestriction) =
    let left = normalize left
    let right = normalize right
    match left, right with
    | Or []       , _             -> right
    | _           , Or []         -> left
    | Or [And rs1], Or [And rs2]  -> And (rs1 @ rs2)
    | Or [h]      , Or [And rs2]  -> And (h :: rs2)
    | Or [And rs1], Or [h]        -> And (h :: rs1)
    | Or [h1]     , Or [h2]       -> And [h1; h2]
    | _                           -> And [] 
    |> simplify

  let or' (left : MAptekaRestriction) (right : MAptekaRestriction) =
    let left = normalize left
    let right = normalize right
    match left, right with
    | Or rs1, Or rs2  -> Or (rs1 @ rs2)
    | _               -> Or [] 
    |> simplify
  
  let rec isMatch (vi : VersionInfo) (mar : MAptekaRestriction) =
    match mar with
    | Exactly vi1 -> vi1 = vi
    | AtLeast vi1 -> Minimum vi1 |> VersionRange.includes (Minimum vi)
    | Not r       -> r |> isMatch vi |> not
    | Or rs       -> rs |> List.exists (isMatch vi)
    | And rs      -> rs |> List.forall (isMatch vi)
  
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

[<AutoOpen>]
module MAptekaRestrictionOperations =
  let inline (.&&) left right = MAptekaRestriction.and' left right
  let inline (.||) left right = MAptekaRestriction.or' left right

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

  override this.ToString() = sprintf "%O %O" this.Name this.VersionRequirement

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




