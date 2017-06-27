namespace MAptekaGet

[<AutoOpen>]
module Domain =
  open System
   
  /// Semantic Version implementation
  [<CustomEquality; CustomComparison>]
  type Version = 
    { Major : uint32
      Minor : uint32
      Patch : uint32
    } 
    override x.ToString() = 
      sprintf "%d.%d.%d" x.Major x.Minor x.Patch

    override x.Equals (that) = 
      match that with
      | :? Version as vi  ->
        x.Major = vi.Major &&
        x.Minor = vi.Minor &&
        x.Patch = vi.Patch
      | _                     ->
        false

    override x.GetHashCode () = hash (x.Major + x.Minor + x.Patch) 

    static member Zero with get () = { Major = 0u; Minor = 0u; Patch = 0u }

    static member (+) (v : Version, a) = { v with Patch = v.Patch + 1u  }
    
    interface System.IComparable with
      member x.CompareTo yobj = 
        match yobj with
        | :? Version as y -> 
          if x.Major <> y.Major then
            compare x.Major y.Major
          elif x.Minor <> y.Minor then
            compare x.Minor y.Minor
          else
            compare x.Patch y.Patch

        | _ -> invalidArg "yobj" "cannot compare values of different types"
  
  type VersionConstraint =
    | VExact  of Version
    | VGt     of Version
    | VLt     of Version
    | VGte    of Version
    | VLte    of Version 
    override this.ToString() =
      match this with
      | VExact v  -> v |> sprintf "= %O"
      | VGt v     -> v |> sprintf "> %O"
      | VLt v     -> v |> sprintf "< %O"
      | VGte v    -> v |> sprintf ">= %O"
      | VLte v    -> v |> sprintf "<= %O"
      
  type VConjunction = VersionConstraint list

  type VDisjunction = VConjunction list
  
  // let showVConjunction (conj: VConjunction) =
  //   String.Join(",", conj)

  // let showVDisjunction (disj: VDisjunction) =
  //   String.Join("||", List.map showVConjunction disj)
  
  type UpdateName =
    | UpdateName of string

    override this.ToString() =
      let (UpdateName name) = this in name
  
  type Constraint =
    | Dependency  of UpdateName * VDisjunction
    | MApteka     of VDisjunction

    override this.ToString() =
      match this with
      | Dependency (name, version) -> sprintf "%O: %O" name version
      | MApteka version            -> sprintf "mapteka: %O" version 

  module VersionConstraint =
    /// no restriction in terms of normal disjuntive form
    let all = [[]]
    /// total restriction in terms of normal disjuntive form
    let never = []
    
    let verify (version: Version) (constr: VersionConstraint) =
      match constr with
      | VExact v  -> version = v
      | VGt v     -> version > v
      | VLt v     -> version < v
      | VGte v    -> version >= v
      | VLte v    -> version <= v
    
    let isSubsetOf (other: VersionConstraint) (constr: VersionConstraint) =
      match constr, other with
      | VExact v, VExact v1   -> v = v1
      | VExact v, VGt v1      -> v > v1
      | VExact v, VLt v1      -> v < v1
      | VExact v, VGte v1     -> v >= v1
      | VExact v, VLte v1     -> v <= v1

      | VGt v, VExact v1   -> false
      | VGt v, VGt v1      -> v >= v1
      | VGt v, VLt v1      -> false
      | VGt v, VGte v1     -> v >= v1
      | VGt v, VLte v1     -> false

      | VLt v, VExact v1   -> false
      | VLt v, VGt v1      -> false
      | VLt v, VLt v1      -> v <= v1
      | VLt v, VGte v1     -> false
      | VLt v, VLte v1     -> v <= v1

      | VGte v, VExact v1   -> false
      | VGte v, VGt v1      -> v > v1
      | VGte v, VLt v1      -> false
      | VGte v, VGte v1     -> v >= v1
      | VGte v, VLte v1     -> false

      | VLte v, VExact v1   -> false
      | VLte v, VGt v1      -> false
      | VLte v, VLt v1      -> v < v1
      | VLte v, VGte v1     -> false
      | VLte v, VLte v1     -> v <= v1

    let isCompatible (that: VersionConstraint) (self: VersionConstraint) =
      if self |> isSubsetOf that
        then true
        else match (self, that) with
              | VGt v1 , VLt v2   -> v1 + 1 < v2
              | VGt v1 , VLte v2  -> v1 < v2
              | VGte v1, VLt v2   -> v1 < v2
              | VGte v1, VLte v2  -> v1 <= v2
              | _                 -> false

    let isConjunctionCompatible (version: Version) =
      not << List.exists (not << (verify version))

    let isDisjunctionCompatible (version: Version) =
      List.exists (isConjunctionCompatible version)
    let inline includes x y = isSubsetOf y x
    let rec isConjunctionSubsetOf (other: VConjunction) (vconj: VConjunction) =
      match vconj with
      | [] ->
          false
      | vc::vcs ->
          match vc with
          | VExact vers -> List.exists ((=) vconstr) vconj | not
      
      
    let isDisjunctionSubsetOf (other: VDisjunction) (vdisj: VDisjunction) =
      vdisj
      |> List.forall (fun vconj ->
        other
        |> List.exists (fun vconj1 ->

        )
      )
    /// >=4.0.0, <4.6.0, >=2.0.0 can be simplified to >=4.0.0, <4.6.0 because >=4.0.0 is a subset of >2.0.0
    let internal removeSubsetLiteralsInAndClause (orClauses: VDisjunction) =
      let simplifyAndClause (andClauses : VConjunction) =
        andClauses
        |> List.filter (fun literal ->
          // we filter out literals, for which another literal exists which is a subset
          andClauses
          |> List.filter ((<>) literal)
          |> List.exists (isSubsetOf literal)
          |> not
        )
      
      in
        List.map simplifyAndClause orClauses
    
    /// >=4.0.0 || <4.6.0 || >=2.0.0 can be simplified to <4.6.0 || >=2.0.0 because >=4.0.0 is a subset of >=2.0.0
    let internal removeSubsetLiteralsInOrClause (orClauses: VDisjunction) =
      let simpleOrLiterals =
        orClauses
        |> List.choose (function [h] -> Some h | _ -> None)
      
      orClauses
      |> List.filter (function
        | [h] ->
          simpleOrLiterals
          |> Seq.filter (fun l -> l <> h)
          |> Seq.exists (fun otherLiteral -> h |> isSubsetOf otherLiteral)
          |> not
        | _ -> true
      )

    /// >=2.0.0, >=4.0.0 || >=2.0.0 can be simplified to >=4.0.0 because any 'and clause' with >=2.0.0 can be removed.
    let internal removeUneccessaryOrClauses (orClauses: VDisjunction) =
      orClauses
      |> List.filter (fun orClause1 ->
        orClauses
        |> List.filter ((<>) orClause1)
        |> List.exists (fun orClause2 ->
          match orClause1, orClause2 with
          | [andClause] , [r]        -> r = andClause
          | [r]         , andClauses -> List.contains r andClauses
          | andClauses1, andClauses2 ->
            List.forall (fun andClause -> List.contains andClause andClauses2) andClauses1  
        )
        |> not
      )

    let internal negate (constr: VersionConstraint) =
      match constr with
      | VExact v  -> [VLt v; VGt v]
      | VGt v     -> [VLte v]
      | VGte v    -> [VLt v]
      | VLt v     -> [VGte v]
      | VLte v    -> [VGt v]

    /// clauses with >=2.0.0, <2.0.0 can be removed because they contains a literal and its negation.
    let internal removeUneccessaryAndClauses : VDisjunction -> VDisjunction =
      List.filter (fun andList ->
        andList
        |> Seq.allPairs andList
        |> Seq.exists (fun (x, y) -> isCompatible x y |> not)
        |> not
      )
      
    /// When we optimized a clause away completely we can replace the hole formula with never.
    let internal replaceWithNoRestrictionIfAnyLiteralListIsEmpty (orClauses: VDisjunction) =
      orClauses
      |> List.exists (function [] -> true | _ -> false)
      |> function true -> all | _ -> orClauses

    let internal simplify (orClauses: VDisjunction) =
      let sortClauses =
        List.map (List.distinct >> List.sort) >> List.distinct >> List.sort 

      let optimize =
        removeSubsetLiteralsInAndClause
        >> removeSubsetLiteralsInOrClause
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

      loop orClauses

    let and' (other : VDisjunction) =
      List.allPairs other
      >> List.map (fun (conjL, conjR) -> conjL @ conjR) 
      >> simplify

    let or' (left : VDisjunction) (right : VDisjunction) =
      simplify (left @ right)


  /// Infix version of VersionConstraint.and'
  let inline (.&&) (op1 : VDisjunction) (op2 : VDisjunction) = VersionConstraint.and' op1 op2

  /// Infix version of VersionConstraint.or'
  let inline (.||) (op1 : VDisjunction) (op2 : VDisjunction) = VersionConstraint.or' op1 op2

  type MAptekaCompatibility =
    | MAptekaCompatibility of VDisjunction
    
  [<CustomEquality; CustomComparison>]
  type Update =
    { Name                : UpdateName 
      Version             : Version
      Constraints         : Constraint list
      Author              : string
      Summary             : string
    }

    override this.Equals (that) = 
      match that with
      | :? Update as that -> 
        this.Name = that.Name && this.Version = that.Version
      | _ -> false

    override this.ToString() =
      sprintf "%O-%O" this.Name this.Version

    override this.GetHashCode() =
      hash (this.Name, this.Version)

    interface System.IComparable with
      member x.CompareTo yobj = 
        match yobj with
        | :? Update as y -> 
          if x.Name <> y.Name then
            compare x.Name y.Name
          else
            compare x.Version y.Version

        | _ ->
          invalidArg "yobj" "cannot compare values of different types"
  
  type Activation =
    | InitialA  of Update
    | ChildA    of Update * Constraint * Activation
  
  type Conflict =
    | MAptekaConflict   of Activation
    // the activation of the incompatible update
    | PrimaryConflict   of Activation
    //  the activation containing the original activation of the
    //  update and the activation of the incompatible
    //  dependency constraint
    | SecondaryConflict of Activation * Activation

  module Parsing =
    open FParsec
    open Utils.Parsing

    let operator =
      choice
        [ pstring "="   >>% VExact
          pstring ">"   >>% VGt
          pstring "<"   >>% VLt
          pstring ">="  >>% VGte
          pstring "<="  >>% VLte
        ]
    
    let version: Parser<Version,unit> =
      let convertToUInt32 txt : Parser<_,_> =
        fun stream ->
          match System.Int32.TryParse txt with
          | true, num when num < 0 -> Reply(Error, expectedString "no negatives!")
          | true, num              -> Reply(uint32 num)
          | false, _               -> Reply(Error, sprintf "couldn't parse %s as number" txt |> expectedString )
      
      let point   = pchar '.'
      let number  = manyChars digit >>= convertToUInt32

      let pMajor  = number <?> "major number"
      let pMinor  = number <?> "minor number"
      let pPatch  = number <?> "patch number"

      (pMajor .>> point .>>. pMinor .>> point .>>. pPatch) 
      |>> (fun ((major, minor), patch) ->
        { Major = major
          Minor = minor
          Patch = patch
        }
      )
    
    let versionConstraint: Parser<VersionConstraint,unit> =
      operator .>> spaces .>>. version .>> spaces
      |>> (fun (f, vi) -> f vi)

    let versionConjunction: Parser<VConjunction,unit> =
      sepBy1 versionConstraint (pchar ',' .>> spaces)

    let versionDisjunction: Parser<VDisjunction,unit> =
      let delimiter =
        pstring "||" .>> spaces
      in
        sepBy1 versionConjunction delimiter
        |>> VersionConstraint.simplify

    let private parseVMap f txt =
      (version <-- txt) |> Result.map f

    let atLeast = parseVMap VGte
    let atMost  = parseVMap VLte
    let exactly = parseVMap VExact
    let updateName =
      let letter = digit
               <|> asciiLetter
               <|> pchar '_'
      
      let toUpdateName = List.toArray
                      >> String
                      >> UpdateName
      
      many letter <?> "update name" |>> toUpdateName

    let dependency = updateName
                  .>> pchar '-'
                  .>>.versionDisjunction
                  .>> spaces

  // module Update =
    // module MR = MAptekaRestriction
    // module VR = VersionRequirement

    // let isCompatibleWithMApteka (mr: MAptekaRestriction) (ui: UpdateInfo) =
    //   ui.MAptekaRestriction |> MR.isSubsetOf mr

    // let isCompatibleWith (ur: UpdateRequirement) (ui: UpdateInfo) =
    //   ur.VersionRequirement |> VR.includes ui.Version 

  // type PushError =
  //   | InvalidUpdateNameFormat           of string
  //   | InvalidVersionFormat              of string
  //   | AlreadyPublished
  //   | UnexpectedVersion                 of VersionInfo * VersionInfo
  //   | InvalidMAptekaRestrictionFormat   of string
  //   | InvalidDependenciesFormat         of string
  //   | MAptekaVersionConflict            of MAptekaRestriction * MAptekaRestriction
  //   | ObtainedCyclicDependency          of VersionInfo * VersionRequirement
  //   | DependencyNotFound                of UpdateName * VersionRequirement
  //   | DependencyVersionConflict         of VersionRequirement * VersionRequirement

