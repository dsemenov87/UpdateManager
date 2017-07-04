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
      | :? Version as vi  -> x.Major = vi.Major && x.Minor = vi.Minor && x.Patch = vi.Patch
      | _                 -> false

    override x.GetHashCode () = hash (x.Major + x.Minor + x.Patch) 
    
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
  
  type RangeOp =
    | Less
    | LessOrEqual
    
    override this.ToString() =
      match this with
      | Less        -> "<"
      | LessOrEqual -> "<="
  
  let isLess<'a when 'a: comparison> (op: RangeOp) : ('a -> 'a -> bool) =
    match op with
    | Less        -> (<)
    | LessOrEqual -> (<=)
    
  type VersionConstraint =
    | Range of Version * RangeOp * RangeOp * Version
    
    override this.ToString() =
      match this with
      | Range (vL, opL, opR, vR) -> sprintf "%O %O v %O %O" vL opL opR vR

  let verifies (version: Version) (constr: VersionConstraint) =
    match constr with
    | Range (vL, opL, opR, vR) ->
      (isLess opL vL version) && (isLess opR version vR) 
  
  type VDisjunction = VersionConstraint list
  
  let isDisjunctionCompatible (version: Version) =
    List.exists (verifies version)

  type UpdateName =
    | UpdateName of string

    override this.ToString() =
      let (UpdateName name) = this in name
  
  type Constraint =
    | Dependency  of UpdateName * VDisjunction

    override this.ToString() =
      match this with
      | Dependency (name, version) -> sprintf "%O: %O" name version
  
  type UpdateSource =
    | Uri of Uri
    | Never

    override this.ToString() =
      match this with
      | Uri uri -> uri.ToString()
      | Never   -> ""
  
  [<CustomEquality; CustomComparison>]
  type Update =
    { Name        : UpdateName 
      Version     : Version
      Constraints : Constraint list
    }

    override this.ToString() =
      sprintf "%O-%O" this.Name this.Version 

    override this.Equals (that) = 
      match that with
      | :? Update as that -> 
        this.Name = that.Name && this.Version = that.Version
      | _ -> false

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
  
  let addConstraint (constr: Constraint) (upd: Update) =
    { upd with Constraints = constr :: upd.Constraints }

  /// bytes
  [<Measure>] type B
  
  type UpdateDetails =
    { Author      : string
      Summary     : string
      Description : string
      ReleaseNotes: string
      Created     : DateTime
      Source      : UpdateSource
      Md5Hash     : string
      Size        : int<B>
    }
  
  type LookupSet = Map<Update, UpdateDetails> 

  type Activation =
    | InitialA  of Update
    | ChildA    of Update * Constraint * parent:Activation

  let activationUpdate (activation: Activation) : Update =
    match activation with
    | InitialA upd      -> upd
    | ChildA (upd,_,_)  -> upd

  let activationParent (activation: Activation) =
    match activation with
    | InitialA _            -> None
    | ChildA (_,_,parent)   -> Some parent
  
  module Parsing =
    open FParsec
    open Utils.Parsing

    let operator =
      pchar '<' >>. opt (pchar '=')
      |>> (fun x -> match x with None -> Less | _ -> LessOrEqual)

    let version: Parser<Version,unit> =
      let convertToUInt32 txt : Parser<_,_> =
        fun stream ->
          match System.Int32.TryParse txt with
          | true, num when num < 0 -> Reply(Error, expectedString "no negatives!")
          | true, num              -> Reply(uint32 num)
          | false, _               -> Reply(Error, sprintf "couldn't parse %s as a number" txt |> expectedString )
      
      let dot     = pchar '.'
      let number  = manyChars digit >>= convertToUInt32

      let pMajor  = number <?> "major number"
      let pMinor  = number <?> "minor number"
      let pPatch  = number <?> "patch number"

      (pMajor .>> dot .>>. pMinor .>> dot .>>. pPatch) 
      |>> (fun ((major, minor), patch) ->
        { Major = major
          Minor = minor
          Patch = patch
        }
      )
    
    let versionConstraint: Parser<VersionConstraint,unit> =
      version .>> spaces .>>. operator .>> spaces .>> pchar 'v'.>> spaces
      .>>. operator .>> spaces .>>. version .>> spaces
      |>> (fun (((vL, opL), opR), vR) -> Range (vL, opL, opR, vR))

    let versionDisjunction: Parser<VDisjunction,unit> =
      let delimiter =
        pchar ',' .>> spaces
      in
        sepBy1 versionConstraint delimiter

    let updateName =
      let letter = digit
               <|> asciiLetter
               <|> pchar '_'
      
      let toUpdateName = List.toArray
                      >> String
                      >> UpdateName
      
      many letter <?> "update name" |>> toUpdateName
    
    let update =
      updateName .>> pchar '-' .>>. version .>> spaces
      |>> (fun (name, vers) -> {Name = name; Version = vers; Constraints = []})

    let dependency = updateName
                  .>> spaces
                  .>> pchar ':'
                  .>> spaces
                  .>>.versionDisjunction
                  .>> spaces
                  |>> Dependency

    let dependencies =
      sepBy1 dependency rf

  /// All possible things that can happen in the use-cases
  type Message =
    | PublishMessage of PublishMessage

  and PublishMessage =
    | BadUpdateFormat   of string
    | BadConstraint     of string // maybe include in BadUpdateFormat?
    | AlreadyPublished  of Update * Version
    | VersionUnexpected of Update * Version
    // todo | CausesCyclicDependency of Update * Version
    | ResolutionMessage of ResolutionResult

  and ResolutionResult = Result<Activation, ResolutionFailure>
  
  and ResolutionFailure =
    | UpdateNotFound          of Activation * UpdateName * suggestions:UpdateName list
    | MissingUpdateVersion    of Activation list
    | IncompatibleConstraints of Activation * Activation
    
  // type UpdateProgram =
  //   | Publish of  name:UpdateName
  //             *   version:Version
  //             *   constraints:Constraint list
  //             *