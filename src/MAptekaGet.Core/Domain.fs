namespace MAptekaGet

#nowarn "0060" // Override implementations in augmentations are now deprecated.

[<AutoOpen>]
module Domain =  
  open System
  module NEL = NonEmptyList
  
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
    
    static member internal Zero =
      { Major = 0u; Minor = 0u; Patch = 0u}

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
    | MApteka

    override this.ToString() =
      match this with
      | UpdateName name -> name
      | MApteka         -> "MApteka"
  
  type Constraint =
    | Dependency  of UpdateName * VDisjunction

    override this.ToString() =
      match this with
      | Dependency (name, version) -> sprintf "%O: %O" name version
  
  type UpdateSource =
    | Uri of Uri

    override this.ToString() =
      match this with
      | Uri uri -> uri.ToString()
  
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

  type UpdateSpecs =
    { Author      : string
      Summary     : string
      Description : string
      ReleaseNotes: string
      Created     : DateTime
    }

  /// bytes
  [<Measure>] type B
  
  type UpdateFileInfo =
    { Source      : UpdateSource
      Md5Hash     : string
      Size        : int<B>
    }

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
                      >> (function "MApteka" -> MApteka | n -> UpdateName n)
      
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
   
  type ValidationResult =
    | Valid   of Update * UpdateSpecs
    | Invalid of string
    
  type VersionCheckResult =
    | CorrectVersion    of Update * UpdateSpecs
    | AlreadyPublished  of Update
    | Unexpected        of Update * Version

  type ResolutionResult =
    | UpdateNotFound          of Activation * UpdateName * suggestions:UpdateName list
    | MissingUpdateVersion    of Activation list
    | IncompatibleConstraints of Activation * Activation
    // todo | CausesCyclicDependency of Update * Version
    | Solution                of Activation list

  type UpdateInfo = Update * UpdateSpecs * UpdateFileInfo

  /// Represents each instruction
  type UpdaterInstruction<'next> = 
    | ValidateInput       of IO.Stream              * (Update * UpdateSpecs -> 'next)
    | CheckVersion        of (Update * UpdateSpecs) * (Update * UpdateSpecs-> 'next)
    | ResolveDependencies of Update list            * (Activation list -> 'next)
    | Publish             of UpdateInfo             * (Update -> 'next)
    | Install             of                        'next
    | Info                of                        'next

  let private mapInstruction f inst  = 
    match inst with
    | ValidateInput (input, next)       -> ValidateInput (input, next >> f)
    | CheckVersion (input, next)        -> CheckVersion (input, next >> f)
    | ResolveDependencies (upds, next)  -> ResolveDependencies (upds, next >> f) 
    | Publish (upd, next)               -> Publish (upd, next >> f)  
    | Install next
    | Info next                         -> failwith "not implemented yet"
      
  /// Represent the Updater Program
  type UpdaterProgram<'a> = 
    | Stop of 'a
    | KeepGoing of UpdaterInstruction<UpdaterProgram<'a>>

  [<RequireQualifiedAccess>]
  module UpdaterProgram =
    let rec bind f = function 
      | KeepGoing instruction -> KeepGoing (mapInstruction (bind f) instruction)
      | Stop x                -> f x

    let validateInput input =
      KeepGoing (ValidateInput (input, Stop))

    let checkVersion input =
      KeepGoing (CheckVersion (input, Stop))

    let resolveDependencies input =
      KeepGoing (ResolveDependencies (input, Stop))

    let publish ufi (upd, updspecs) =
      KeepGoing (Publish ((upd, updspecs, ufi), Stop))

  module Reporting =
    open PrettyPrint
    module Rep = Utils.Reporting

    let rec internal treeFromActivation (act: Activation) =
      let showNode =
        sprintf "%O => %O"
      let rec showUpToRoot constr act =
        seq {
          match act with
          | InitialA upd ->
              yield showNode upd constr
          | ChildA (upd, nextConstr, constrAct) ->
              yield showNode upd constr
              yield! showUpToRoot nextConstr constrAct
        }
      in
        match act with
        | InitialA upd -> NEL.singleton (show upd)
        | ChildA (upd, lastConstr, constrAct) ->
            NEL.create (show upd) (Seq.toList (showUpToRoot lastConstr constrAct))
            |> NEL.reverse
        |> Tree.fromNonEmptyList

    let internal resolutionToMsg (res: ResolutionResult) =
      match res with
      | Solution acts ->
          Rep.Message.New
            ( "The following updates can be published."
            )
            [ List.map (txt << show << activationUpdate) acts |> vcat |> indent 4  ]
            Rep.Success

      | UpdateNotFound (_, updName, suggestions) ->
          Rep.Message.New
            ( "Could not find any updates named '" + updName.ToString() + "'."
            )
            [ txt "Here are some updates that have similar names:"
              List.map (txt << show) suggestions |> vcat |> indent 4
              txt "Maybe you want one of those?"
            ]
            Rep.Error

      | IncompatibleConstraints (act1, act2) ->
          Rep.Message.New
            ( "There was a conflict for update version.")
            [ Rep.drawTree (treeFromActivation act1) |> indent 4
              Rep.drawTree (treeFromActivation act2) |> indent 4
            ]
            Rep.Error


      | MissingUpdateVersion acts ->
          let docTree, update =
            match acts with
            | [] -> Doc.Empty, "<not resolved>"
            | act::_ ->
                let update = (activationUpdate act).ToString()
                match (activationParent act) with
                | None -> txt update, update 
                | Some parentAct ->
                    let tree =
                      Node (parentAct, acts |> Seq.map Tree.singleton)
                      |> Tree.map (fun node -> (activationUpdate node).ToString())
                    in
                      Rep.drawTree tree, update

          Rep.Message.New
            ( "Cannot resolve dependencies. Missing update version '" + update + "'."
            )
            [ docTree |> indent 4 ]
            Rep.Error

    let internal validationToMsg (res: ValidationResult) =
      match res with
      | Valid (upd,_) ->
          Rep.Message.New ("The update name '" + (show upd) + "' is valid.") [] Rep.Success
      | Invalid problem ->
          Rep.Message.New "The update name is invalid." [ txt problem ] Rep.Error

    let internal versionCheckToMsg (res: VersionCheckResult) =
      match res with
      | CorrectVersion (upd,_) ->
          Rep.Message.New
            ( "Update version '" + (show upd) + "' is good."
            )
            []
            Rep.Success         
      
      | AlreadyPublished (upd) ->
          Rep.Message.New
            ( "Update '" + (show upd) + "' has already been published. You cannot publish it again!."
            )
            []
            Rep.Error

      | Unexpected (upd, version) ->
          Rep.Message.New
            ( "You cannot publish a package with an unexpected version.\n" +
              "The next version should be greater then '" + (show version) + "'."
            )
            []
            Rep.Error
    
 
  module Operations =
    let inline (>>=) x f = UpdaterProgram.bind f x 
 