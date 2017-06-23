namespace MAptekaGet

[<AutoOpen>]
module Utils =
  let internal memoize (f: 'a -> 'b) : 'a -> 'b =
    let cache = System.Collections.Concurrent.ConcurrentDictionary<'a, 'b>()
    fun (x: 'a) ->
      cache.GetOrAdd(x, f)

  let inline (++) x y = match x with None -> y | _ -> x

  module Parsing =
    open FParsec

    let inline hyphen<'a> = pchar '-'

    /// applies the parser p, ignores the result, and returns x.
    let inline (>>%) p x = p |>> (fun _ -> x)

    let toResult text parser =
      let res = run parser text
      match res with
      | Success (res,_,_) -> Result.Ok res
      | Failure _         -> Result.Error (sprintf "%A" res)

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
    
    interface System.IComparable with
      member x.CompareTo yobj = 
        match yobj with
        | :? Version as y -> 
          if x.Major <> y.Major then
            compare x.Major y.Major
          else if x.Minor <> y.Minor then
            compare x.Minor y.Minor
          else
            compare x.Patch y.Patch

        | _ -> invalidArg "yobj" "cannot compare values of different types"
  
  type VersionInfo = Version

  module Version =
    open FParsec
    open Parsing

    let internal parser =
      let convertToUInt32 txt : Parser<_,_> =
        fun stream ->
          match Int32.TryParse txt with
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
      
    let parse (text : string) = 
      let res =
        run parser text
      match res with
      | Success (res,_,_) -> Result.Ok res
      | Failure _         -> Result.Error (sprintf "%A" res)
      
    let zero = { Major = 0u; Minor = 0u; Patch = 0u }

    // let sortVersions =
    //   Array.choose (fun v -> try Some(v,parse v) with | _ -> None)
    //   >> Array.sortBy snd
    //   >> Array.map fst
    //   >> Array.rev

  type VersionRangeBound =
    | Excluding
    | Including

  type VersionRange =
    | Minimum     of VersionInfo
    | GreaterThan of VersionInfo
    | Maximum     of VersionInfo
    | LessThan    of VersionInfo
    | Specific    of VersionInfo
    | Range       of fromB  : VersionRangeBound
                   * from   : VersionInfo
                   * _to    : VersionInfo
                   * _toB   : VersionRangeBound

    override this.ToString() =
      match this with
      | Specific v    -> v.ToString()
      | Minimum v     -> v |> sprintf ">= %O"
      | GreaterThan v -> v |> sprintf "> %O"
      | Maximum v     -> v |> sprintf "<= %O"
      | LessThan v    -> v |> sprintf "< %O"
      | Range(fromB, from, _to, _toB) ->
        let from =
          match fromB with
          | Excluding -> from |> sprintf "> %O"
          | Including -> from |> sprintf ">= %O"

        let _to =
          match _toB with
          | Excluding -> _to |> sprintf "< %O"
          | Including -> _to |> sprintf "<= %O"

        from + " " + _to

  module VersionRange =
    let atLeast = VersionInfo.parse >> Result.map Minimum
    let atMost = VersionInfo.parse >> Result.map Maximum
    let exactly = VersionInfo.parse >> Result.map Specific
    let between (version1, version2) =
      let lb = VersionInfo.parse version1
      let rb = VersionInfo.parse version2
      match lb, rb with
      | Ok lb', Ok rb' ->
        Ok (Range(VersionRangeBound.Including, lb', rb', VersionRangeBound.Excluding))
      
      | Ok _, Error err
      | Error err, Ok _->
        Error err

      | Error err1, Error err2 ->
        Error (err1 + ", " + err2)

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

  type VersionConstraint =
    | VExact  of Version
    | VGt     of Version
    | VLt     of Version
    | VGte    of Version
    | VLte    of Version

  module VersionConstraint =
    open FParsec
    open Parsing

    let internal parser =
      let operator =
        choice
          [ pstring "="   |>> fun _ -> VExact
            pstring ">"   |>> fun _ -> VGt
            pstring "<"   |>> fun _ -> VLt
            pstring ">="  |>> fun _ -> VGte
            pstring "<="  |>> fun _ -> VLte
          ]

      let simpleConstraint =
        operator .>> spaces .>>. Version.parser .>> spaces
        |>> (fun (f, vi) -> f vi)
      
      let bound =
        left .>>. VersionInfo.parser .>> spaces .>> pchar ',' .>> spaces
        .>>. VersionInfo.parser .>> spaces .>>. right .>> spaces
        |>> (fun (((vb1, vi1), vi2), vb2) ->
            Range (vb1, vi1, vi2, vb2) |> VersionRequirement
        )

      operatorRequirement <|> bound 

    let parse (text:string) =
      parser |> toResult text



  type VConjunction = VersionConstraint list
  type VDisjunction = VConjunction list

  type MAptekaRestriction =
    | Exactly of VersionInfo
    | AtLeast of VersionInfo
    | Not     of MAptekaRestriction
    | Or      of MAptekaRestriction list
    | And     of MAptekaRestriction list

    override this.ToString() =
      match this with
      | Exactly r       -> sprintf "= %O" r
      | AtLeast r       -> sprintf ">= %O" r
      | Not (AtLeast r) -> sprintf "< %O" r
      | Not fr          -> sprintf "NOT (%O)" fr
      | Or rl           ->
        match rl with
        | []        -> "-"
        | [single]  -> sprintf "%O" single
        | _         -> sprintf "OR %s" (System.String.Join(" ", rl |> Seq.map (sprintf "(%O)")))
      | And rl ->
        match rl with
        | []        -> "*"
        | [single]  -> sprintf "%O" single
        | _         -> sprintf "AND %s" (System.String.Join(" ", rl |> Seq.map (sprintf "(%O)")))

  module MAptekaRestriction =
    open FParsec
    open Parsing

    let noRestriction = AtLeast VersionInfo.zero
    let empty = Not noRestriction
    let rec negate : MAptekaRestriction -> MAptekaRestriction = function
      | Or rs   -> And (rs |> List.map negate)
      | And rs  -> Or (rs |> List.map negate)
      | Not r   -> r
      | r       -> Not r  

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
      | Not (Or xs) -> (And (xs |> List.map negate)) |> isSubsetOf y
      | Not (And xs) -> (Or (xs |> List.map negate)) |> isSubsetOf y

      | Or xs -> xs |> List.forall (fun xi -> xi |> isSubsetOf y)
      | And xs -> xs |> List.exists (fun xi -> xi |> isSubsetOf y)

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
          calc (rs |> List.map negate |> And)

        | Not (And rs) ->
          calc (rs |> List.map negate |> Or)

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
        
    
    
    /// (>=4.0.0) && (<4.6.0) && (>=2.0.0) can be simplified to (<4.6.0) && (>=4.0.0) because (>=4.0.0) is a subset of (>2.0.0)
    let internal removeSubsetLiteralsInAndClause (mr: MAptekaRestriction) =
      match mr with
      | Or orClauses ->
        let simplifyAndClause (andClauses : MAptekaRestriction list) =
          andClauses
          |> List.filter (fun literal ->
            // we filter out literals, for which another literal exists which is a subset
            andClauses
            |> List.filter ((<>) literal)
            |> List.exists (isSubsetOf literal)
            |> not
          )
        
        orClauses
        |> List.map (function
          | And andClauses -> simplifyAndClause andClauses |> And
          | r -> r
        ) 
        |> Or
      | mr -> mr
    
    /// (>=4.0.0) || (<4.6.0) || (>=2.0.0) can be simplified to (<4.6.0) || (>=2.0.0) because (>=4.0.0) is a subset of (>=2.0.0)
    let internal removeSubsetLiteralsInOrClause (mr: MAptekaRestriction) =
      match mr with
      | Or orClauses ->
        let simpleOrLiterals =
          orClauses
          |> List.choose (function And [h] -> Some h | _ -> None)
        
        orClauses
        |> List.filter (function
          | And [h] ->
            simpleOrLiterals
            |> Seq.filter (fun l -> l <> h)
            |> Seq.exists (fun otherLiteral -> h |> isSubsetOf otherLiteral)
            |> not
          | _ -> true
        )
        |> Or
      | _ -> mr

    /// ((>=2.0.0) && (>=4.0.0)) || (>=2.0.0) can be simplified to (>=4.0.0) because any AND clause with (>=2.0.0) can be removed.
    let internal removeUneccessaryOrClauses (mr: MAptekaRestriction) =
      match mr with
      | Or orClauses ->
        orClauses
        |> List.filter (fun orClause1 ->
          orClauses
          |> List.filter ((<>) orClause1)
          |> List.exists (fun orClause2 ->
            match orClause1, orClause2 with
            | And andClauses1, And andClauses2 ->
              andClauses1 |> List.forall (fun andClause -> andClauses2 |> List.contains andClause)
            | And [andClause], r ->
              r = andClause
            | r, And andClauses ->
              andClauses |> List.contains r
            | _ ->
              false
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
        |> List.map (function And andClauses  -> andClauses | r -> [r])
        |> List.filter (fun andList ->
          andList
          |> List.map (negate >> negate) // remove double not, example Not(Not x) -> x 
          |> List.exists (fun r -> andList |> List.contains (negate r))
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

    let rec internal replaceDoubleNot (mr : MAptekaRestriction) =
      match mr with
      | (Not(Not r))  -> r
      | And rs        -> rs |> List.map replaceDoubleNot |> And
      | Or rs         -> rs |> List.map replaceDoubleNot |> Or
      | _             -> mr

    let internal simplify (mr: MAptekaRestriction) =
      let sortClauses (mr: MAptekaRestriction) =
        match mr with
        | Or orClauses ->
          orClauses
          |> List.map (function And andClauses -> andClauses |> List.distinct |> List.sort |> And | r -> r)
          |> List.distinct
          |> List.sort 
          |> function [x] -> x | xs -> Or xs
        | _ ->
          mr

      let optimize =
        normalize
        >> removeNegatedLiteralsWhichOccurSinglePositive
        >> removeSubsetLiteralsInAndClause
        >> removeSubsetLiteralsInOrClause
        >> removeUneccessaryAndClauses
        >> removeUneccessaryOrClauses
        >> replaceWithNoRestrictionIfAnyLiteralListIsEmpty
        >> replaceDoubleNot
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
      | Or []       , _             -> Or []
      | _           , Or []         -> Or []
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

    let satisfy (another : MAptekaRestriction) (one : MAptekaRestriction) =
      and' one another <> empty

    let parse (text:string) =
      let true' =
        pstring "TRUE"  >>% noRestriction
      let false' =
        pstring "FALSE" >>% empty

      let operator =
        choice
          [ pstring "="   |>> fun _ -> Exactly
            pstring ">="  |>> fun _ -> AtLeast
            pstring "<"   |>> fun _ -> Not << AtLeast
          ]
        <?> "operator (=, <, >=)"
      
      let pValue, pValueRef =
        createParserForwardedToRef<MAptekaRestriction, _>()
        
      let restriction = operator
                      .>> spaces
                      .>>. VersionInfo.parser
                      .>> spaces
                      |>> fun (f, vi) -> f vi
      
      let value  = pValue .>> spaces
      let left'  = pchar '('     .>> spaces  <?> "left bracket '('"
      let right' = pchar ')'     .>> spaces  <?> "right bracket ')'"
      let and'   = pstring "AND" .>> spaces  <?> "keyword 'AND'"
      let or'    = pstring "OR"  .>> spaces  <?> "keyword 'OR'"
      let not'   = pstring "NOT" .>> spaces  <?> "keyword 'NOT'"

      let block =
        let values =
          value .>>. opt ((or' >>. (sepBy1 value or') |>> Or) <|> (and' >>. (sepBy1 value and') |>> And))
          |>> fun (r, rest) ->
            match rest with
            | Some (Or rs)  -> Or (r::rs)
            | Some (And rs) -> And (r::rs)
            | _             -> r
        between left' right' values

      let notBlock =
        not' >>. block |>> Not

      pValueRef := choice 
        [ true' 
          false'
          restriction
          block
          notBlock
        ]
      
      pValue |> toResult text
      

      // let rec parseOperator (text:string) =
      //   match text.Trim() with
      //   | t when String.IsNullOrEmpty t -> failwithf "trying to parse an otherator but got no content"
      //   | h when h.StartsWith "(" || h.StartsWith ")" -> parseOperator (h.Substring 1)
      //   | h when h.StartsWith ">=" || h.StartsWith "=" || h.StartsWith "<" ->
      //     // parse >= 
      //     let smallerThan = h.StartsWith "<"
      //     let isEquals = h.StartsWith "="
      //     let rest = h.Substring (if smallerThan || isEquals then 1 else 2)
      //     let splitted = rest.TrimStart().Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
      //     if splitted.Length < 1 then failwithf "No parameter after >= or < in '%s'" text
      //     let rawOperator = splitted.[0]
      //     let operator = rawOperator.TrimEnd([|')'|])
      //     try
      //       let version = VersionInfo.parse operator
      //       let f = 
      //         if smallerThan then Not << AtLeast
      //         elif isEquals then Exactly
      //         else AtLeast
      //       let operatorIndex = text.IndexOf operator
      //       f version, text.Substring(operatorIndex + operator.Length)
      //     with ex ->
      //       failwithf "invalid parameter '%s' after >= or < in '%s'" operator text 

      //   | h when h.StartsWith "AND" || h.StartsWith "OR" ->
      //     let isAnd = h.StartsWith "AND"
      //     let next = if isAnd then h.Substring 3 else h.Substring 2
      //     let rec parseOperand cur (next:string) =
      //       let trimmed = next.TrimStart()
      //       if trimmed.StartsWith "(" then
      //         let operand, remaining = parseOperator (trimmed.Substring 1)
      //         let remaining = remaining.TrimStart()
      //         if remaining.StartsWith ")" |> not then failwithf "expected ')' after operand, '%s'" text
      //         parseOperand (operand::cur) (remaining.Substring 1)
      //       else
      //         cur, next
          
      //     let operands, next = parseOperand [] next
      //     if operands.Length = 0 then failwithf "Operand '%s' without argument is invalid in '%s'" (h.Substring (0, 2)) text
      //     let f, def = if isAnd then And, noRestriction else Or, totalRestriction
      //     operands |> f, next
      //   | h when h.StartsWith "NOT" ->
      //     let next = h.Substring 2
          
      //     if next.TrimStart().StartsWith "(" then
      //       let operand, remaining = parseOperator (next.Substring 1)
      //       let remaining = remaining.TrimStart()
      //       if remaining.StartsWith ")" |> not then failwithf "expected ')' after operand, '%s'" text
      //       let next = remaining.Substring 1
      //       negate operand, next
      //     else
      //       failwithf "Expected operand after NOT, '%s'" text
      //   | h when h.StartsWith "*" ->
      //       let rest = (h.Substring 1).TrimStart()
      //       noRestriction, rest
      //   | h when h.StartsWith "-" ->
      //       let rest = (h.Substring 1).TrimStart()
      //       totalRestriction, rest
      //   | _ ->
      //       failwithf "Expected operator, but got '%s'" text
              
      // let result, next = parseOperator text
      // if String.IsNullOrEmpty next |> not then
      //   failwithf "Successfully parsed '%O' but got additional text '%s'" result next
      // result

  [<AutoOpen>]
  module MAptekaRestrictionOperators =
    /// Infix version of MAptekaRestriction.and'
    let inline (.&&) left right = MAptekaRestriction.and' left right
    /// Infix version of MAptekaRestriction.or'
    let inline (.||) left right = MAptekaRestriction.or' left right

  type VersionRequirement =
    | VersionRequirement of VersionRange

    override this.ToString() =
      let (VersionRequirement range) = this in range.ToString()

  module VersionRequirement =
    open FParsec
    open Utils.Parsing
    
    /// Checks wether the given version is in the version range
    let isInRange (version : VersionInfo) (VersionRequirement range) =
      let sameVersion v=
        v.Major = version.Major && v.Minor = version.Minor && v.Patch = version.Patch

      match range with
      | Specific v    -> v = version || sameVersion v
      | Minimum v     -> v = version || (v <= version) || sameVersion v
      | GreaterThan v -> v < version
      | Maximum v     -> v = version || (v >= version)
      | LessThan v    -> v > version && not (sameVersion v)
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
    
    /// Pipeline version of isInRange
    let includes version requirement =
      isInRange version requirement

    let range (VersionRequirement range) = range

    let all = VersionRequirement(Minimum(VersionInfo.zero))

    let internal parser =
      let operator =
        choice
          [ pstring "="   |>> fun _ -> Specific
            pstring ">"   |>> fun _ -> GreaterThan
            pstring ">="  |>> fun _ -> Minimum
            pstring "<"   |>> fun _ -> LessThan
            pstring "<="  |>> fun _ -> Maximum
          ]
        <?> "operator (=, <, >=, >, <=)"

      let operatorRequirement =
        operator .>> spaces .>>. VersionInfo.parser .>> spaces
        |>> (fun (f, vi) -> VersionRequirement (f vi))

      let left =
        (pchar '[' |>> fun _ -> Including) <|> (pchar '(' |>> fun _ -> Excluding)

      let right =
        (pchar ']' |>> fun _ -> Including) <|> (pchar ')' |>> fun _ -> Excluding)
      
      let bound =
        left .>>. VersionInfo.parser .>> spaces .>> pchar ',' .>> spaces
        .>>. VersionInfo.parser .>> spaces .>>. right .>> spaces
        |>> (fun (((vb1, vi1), vi2), vb2) ->
            Range (vb1, vi1, vi2, vb2) |> VersionRequirement
        )

      operatorRequirement <|> bound 

    let parse (text:string) =
      parser |> toResult text

 
  
  type UpdateName =
    | UpdateName of string

    override this.ToString() =
      let (UpdateName name) = this in name

  module UpdateName =
    open FParsec
    open Parsing

    let internal parser =
      let letter = digit
                <|> asciiLetter
                <|> pchar '-'
      
      let toUpdateName = List.toArray
                      >> String
                      >> UpdateName
      
      many letter
      <?> "update name"
      |>> toUpdateName

    let parse (txt: string) =
      parser |> toResult txt
  
  type Dependency =
    | Dependency of UpdateName * VDisjunction

    override this.ToString() =
      let (Dependency (name, version)) = this
      in sprintf "%O %O" name version

  type MAptekaCompatibility =
    | MAptekaCompatibility of VDisjunction
     
  type UpdateRequirement =
    { Name              : UpdateName
      VersionRequirement: VersionRequirement
    }

    override this.ToString() =
      sprintf "%O-%O" this.Name this.VersionRequirement

  module UpdateRequirement =
    open FParsec
    open Parsing

    let internal parser = UpdateName.parser
                        .>> pchar '-'
                        .>>.VersionRequirement.parser
                        .>> spaces

    let parse (text: string) =
      parser |> toResult text

  type DependencySet = UpdateRequirement Set

  module DependencySet =
    open FParsec
    open Parsing

    let internal parser =
      let delimiter =
        opt (pchar '\r') .>>. pchar '\n'

      sepEndBy1 UpdateRequirement.parser delimiter
      |>> Set.ofList
    
    let parse (text: string) =
      parser |> toResult text


  [<CustomEquality; CustomComparison>]
  type Update =
    { Name                : UpdateName 
      Version             : Version
      MAptekaCompatibility: MAptekaCompatibility
      Dependencies        : Dependency list
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

  type UpdateInfo = Update
  
  module Update =
    module MR = MAptekaRestriction
    module VR = VersionRequirement

    let isCompatibleWithMApteka (mr: MAptekaRestriction) (ui: UpdateInfo) =
      ui.MAptekaRestriction |> MR.isSubsetOf mr

    let isCompatibleWith (ur: UpdateRequirement) (ui: UpdateInfo) =
      ur.VersionRequirement |> VR.includes ui.Version 

  type PushError =
    | InvalidUpdateNameFormat           of string
    | InvalidVersionFormat              of string
    | AlreadyPublished
    | UnexpectedVersion                 of VersionInfo * VersionInfo
    | InvalidMAptekaRestrictionFormat   of string
    | InvalidDependenciesFormat         of string
    | MAptekaVersionConflict            of MAptekaRestriction * MAptekaRestriction
    | ObtainedCyclicDependency          of VersionInfo * VersionRequirement
    | DependencyNotFound                of UpdateName * VersionRequirement
    | DependencyVersionConflict         of VersionRequirement * VersionRequirement

