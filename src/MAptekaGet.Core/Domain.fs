namespace MAptekaGet

#nowarn "0060" // Override implementations in augmentations are now deprecated.

[<AutoOpen>]
module Domain =  
  open System
  open System.Collections.Generic
  
  module NEL = NonEmptyList
  module Rep = Utils.Reporting
  
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

  let updName {Name=name} = name 
  
  let addConstraint (constr: Constraint) (upd: Update) =
    { upd with Constraints = constr :: upd.Constraints }

  type UpdateSpecs =
    { Author      : string
      Summary     : string
      UniqueCode  : string
      Description : string
      ReleaseNotes: string
      Created     : DateTime
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
      updateName .>> pchar '-' .>>. version .>> spaces .>> eof
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
   

  type VersionCheckResult =
    | CorrectVersion    of Update
    | AlreadyPublished  of Update
    | UnexpectedVersion of Update * Version

  let cheskUpdateVersion lookupSet upd =
    match lookupSet
          |> Seq.map (fun u -> u.Version) 
          |> Seq.filter ((>=) upd.Version)
          |> Seq.sortDescending
          |> Seq.tryHead with
    | Some v when upd.Version > v ->
        UnexpectedVersion (upd, v)
    
    | Some v ->
        AlreadyPublished upd
    
    | None ->
        CorrectVersion upd

  type ResolutionResult =
    | UpdateNotFound          of UpdateName * suggestions:UpdateName list
    | MissingUpdateVersion    of Activation
    | IncompatibleConstraints of Activation * Activation
    | Solution                of Tree<Update> list

  type PublishResult =
    | Published       of Update
    | MissingFileBody of Update

  type EscId = Guid

  type EscFileInfo = Uri * EscId

  type AvailableResponse =
    | ListAvailable of EscFileInfo list
    | EscNotFound   of Update

  type ConvertToEscResult =
    | EmptyUpdateList
    | Converted                 of Update Set * EscId
    | ConvertionSourceNotFound  of Update Set

  type UpdateInfo = Update * UpdateSpecs

  type CustomerId = string

  type User =
    | Issuer of CustomerId

  type DomainMessage =
    | Never
    | ValidationMessage     of Result<Update Set, string>
    | VersionCheckMessage   of VersionCheckResult
    | ResolutionMessage     of ResolutionResult
    | AvailableMessage      of AvailableResponse
    | ConvertToEscMessage   of ConvertToEscResult
    | PublishMessage        of PublishResult
    | AcceptDownloadingMessage    
                            of Result<EscId, string>
    | UnexpectedError       of string

  /// Represents each instruction
  type UpdaterInstruction<'next> =
    | Authorize           of                (User -> 'next)
    | ValidateUpdate      of                (Update -> 'next)
    | ReadSpecs           of                (UpdateSpecs -> 'next)
    | ReadUserUpdates     of                (Update Set * CustomerId -> 'next)
    | ReadEscUri          of                (EscId -> 'next)
    | CheckVersion        of Update       * (Update -> 'next)
    | ResolveDependencies of Update Set   * (Tree<Update> list -> 'next)
    | Publish             of UpdateInfo   * (UpdateInfo -> 'next)
    | GetAvailableUpdates of User         * (EscFileInfo list  -> 'next)
    | ConvertToEsc        of CustomerId * Update Set
                                          * (EscFileInfo -> 'next)
    | PrepareToInstall    of CustomerId * Update Set
                                          * 'next
    | AcceptDownloading   of CustomerId * EscId
                                          * 'next

  let private mapInstruction f inst  = 
    match inst with
    | Authorize next                      -> Authorize (next >> f)
    | ValidateUpdate next                 -> ValidateUpdate (next >> f)
    | ReadSpecs next                      -> ReadSpecs (next >> f)
    | ReadUserUpdates next                -> ReadUserUpdates (next >> f)
    | ReadEscUri next                     -> ReadEscUri (next >> f)
    | CheckVersion (input, next)          -> CheckVersion (input, next >> f)
    | ResolveDependencies (upds, next)    -> ResolveDependencies (upds, next >> f) 
    | Publish (ui, next)                  -> Publish (ui, next >> f)
    | ConvertToEsc (cid, upds, next)      -> ConvertToEsc (cid, upds, next >> f)
    | PrepareToInstall (cid, upds, next)  -> PrepareToInstall (cid, upds, next |> f)
    | GetAvailableUpdates (user, next)    -> GetAvailableUpdates (user, next >> f)
    | AcceptDownloading (cid, escId, next) -> AcceptDownloading (cid, escId, next |> f)

  /// Represent the Updater Program
  type UpdaterProgram<'a> = 
    | Stop    of 'a
    | OrElse  of UpdaterProgram<'a> * UpdaterProgram<'a>
    | AndThen of UpdaterInstruction<UpdaterProgram<'a>>

  [<RequireQualifiedAccess>]
  module UpdaterProgram =
    let rec bind f = function 
      | Stop x              -> f x
      | OrElse (p1, p2)     -> OrElse (bind f p1, bind f p2)           
      | AndThen instruction -> AndThen (mapInstruction (bind f) instruction)

    let inline orElse p1 p2 = OrElse (p1, p2)
    let inline choice progs = Seq.reduce orElse progs 

    let authorize =
      AndThen (Authorize (Stop))
    
    let readSpecs =
      AndThen (ReadSpecs (Stop))

    let readUserUpdates =
      AndThen (ReadUserUpdates (Stop))

    let readEscUri =
      AndThen (ReadEscUri (Stop))

    let validateUpdate =
      AndThen (ValidateUpdate (Stop))    

    let checkVersion input =
      AndThen (CheckVersion (input, Stop))

    let resolveDependencies input =
      AndThen (ResolveDependencies (input, Stop))

    let publish uis =
      AndThen (Publish (uis, Stop))
     
    let convertToEsc cid upd =
      AndThen (ConvertToEsc (cid, upd, Stop))

    let prepareToInstall cid upd =
      AndThen (PrepareToInstall (cid, upd, Stop ()))

    let availableUpdates cid =
      AndThen (GetAvailableUpdates (cid, Stop))

    let acceptDownloading cid escId =
      AndThen (AcceptDownloading (cid, escId, Stop ()))

    let ignore _ = Stop ()

  module internal Json =
    open Chiron
    
    let escFileInfoToJson ((uri, md5): EscFileInfo) =
      [ ("Url", string uri)
        ("Hash", md5.ToString("N").ToUpper())
      ]
      |> Map.ofList
      |> JsonObject.ofMapWith Json.String
      |> Object

    let jsonMessagesToSingleJson jmsgs =
      ["data", Json.Array jmsgs]
      |> Map.ofList
      |> JsonObject.ofMap |> Object
  
  [<AutoOpen>]
  module internal Printing =
    open PrettyPrint
    open Chiron
    
    let rec internal treeFromActivation (act: Activation) =
      
      let rec showUpToRoot constr act =
        seq {
          match act with
          | InitialA upd ->
              yield Rep.showNode upd constr
          | ChildA (upd, nextConstr, constrAct) ->
              yield Rep.showNode upd constr
              yield! showUpToRoot nextConstr constrAct
        }
      in
        match act with
        | InitialA upd -> NEL.singleton (string upd)
        | ChildA (upd, lastConstr, constrAct) ->
            NEL.create (string upd) (Seq.toList (showUpToRoot lastConstr constrAct))
            |> NEL.reverse
        |> Tree.fromNonEmptyList

    let publishResultToPrintMessage (msg: PublishResult) =
      match msg with
      | Published upd ->
          Rep.Message.New
            ( sprintf "Update %O is published." upd
            )
            []
      
      | MissingFileBody upd ->
          Rep.Message.New
            ( sprintf "File body for update '%O' is missed." upd
            )
            []

    let availableToPrintMessage (msg: AvailableResponse) =
      match msg with
      | ListAvailable efis ->
          efis
          |> List.map Json.escFileInfoToJson
          |> Json.jsonMessagesToSingleJson
          |> PrettyPrint.Json.pretty

      | EscNotFound upd ->
          Rep.Message.New
            ( "Cannot find *.esc file for update '" + string upd + "'."
            )
            []
            |> Rep.pmsgToDoc

    let resolutionToPrintMessage (msg: ResolutionResult) =
      match msg with
      | Solution forest ->
        Rep.Message.New
          ( "The update dependencies:"
          )
          ( forest |> List.map (Tree.map string >> Rep.drawTree))
          

      | UpdateNotFound (updName, suggestions) ->
          Rep.Message.New
            ( "Could not find any update named '" + string updName + "'. Maybe you want one of those?"
            )
            [ List.map (txt << string) suggestions |> vcat
            ]

      | IncompatibleConstraints (act1, act2) ->
          Rep.Message.New
            ( "There was a conflict for update version:")
            [ Rep.drawTree (treeFromActivation act1)
              Rep.drawTree (treeFromActivation act2) 
            ]

      | MissingUpdateVersion act ->
          let docTree, update =
            let update = act |> activationUpdate |> string
            match act with
            | InitialA _ -> txt update, update 
            | ChildA (_, constr, parentAct) ->
                let tree =
                  Node (Rep.showNode (activationUpdate parentAct) constr, seq [update |> string |> Tree.singleton])
                  // |> Tree.map (activationUpdate >> string)
                in
                  Rep.drawTree tree, update

          Rep.Message.New
            ( "Cannot resolve dependencies. Missing update version."
            )
            [ docTree ]

    let convertionEscToPrintMessage (msg: ConvertToEscResult) =
      let updsToPrintForm upds =
        upds |> Seq.map string |> Seq.toList

      match msg with
      | EmptyUpdateList ->
          Rep.Message.New "Empty update list." []

      | Converted (upds, escId) ->
          Rep.Message.New
            ( sprintf "Updates %A are converted to *.esc file '%O' successfully." (updsToPrintForm upds) escId
            )
            []

      | ConvertionSourceNotFound upds ->
          Rep.Message.New
            ( sprintf "Cannot find *.upd file for one of following updates: %A." (updsToPrintForm upds)
            )
            []

    let versionCheckToPrintMessage (msg: VersionCheckResult) =
      match msg with
      | CorrectVersion upd ->
          Rep.Message.New
            ( "Update version '" + (string upd) + "' is correct."
            )
            []       
      
      | AlreadyPublished upd ->
          Rep.Message.New
            ( "Update '" + (string upd) + "' has already been published. You cannot publish it again!"
            )
            []

      | UnexpectedVersion (upd, version) ->
          Rep.Message.New
            ( "The next version should be greater then '" + (string version) + "'."
            )
            []

  type DomainMessage with

    override dmsg.ToString () =
      let doc =
        match dmsg with
        | Never -> Rep.Message.Zero |> Rep.pmsgToDoc
        
        | ValidationMessage msg ->
            msg
            |> Result.map (sprintf "%A.")
            |> Rep.resultToMsg
            |> Rep.pmsgToDoc

        | ResolutionMessage resmsg ->       
            resolutionToPrintMessage resmsg |> Rep.pmsgToDoc

        | VersionCheckMessage vcmsg ->
            versionCheckToPrintMessage vcmsg |> Rep.pmsgToDoc

        | AvailableMessage msg ->
            availableToPrintMessage msg

        | ConvertToEscMessage msg ->
            convertionEscToPrintMessage msg |> Rep.pmsgToDoc
            
        | PublishMessage msg ->
            msg
            |> publishResultToPrintMessage
            |> Rep.pmsgToDoc

        | AcceptDownloadingMessage msg ->
            msg
            |> Result.map (sprintf "Esc file '%O' downloading is accepted.")
            |> Rep.resultToMsg
            |> Rep.pmsgToDoc

        | UnexpectedError err ->
            err |> PrettyPrint.PPrint.txt
      
      in
        doc |> PrettyPrint.PPrint.render ^ Some 78
      
  module Operations =
    let inline (>>=) x f = UpdaterProgram.bind f x 
 