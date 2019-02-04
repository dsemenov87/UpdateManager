[<AutoOpen>]
module Domain

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

type VDisjunction = NEL<VersionConstraint>

let isDisjunctionCompatible (version: Version) =
  Seq.exists (verifies version)

type UpdateName =
  | UpdateName of string
  | Platform

  override __.ToString() =
    match __ with
    | UpdateName name -> name
    | Platform        -> "Platform"

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
  open Prelude.Parsing

  let operator =
    pchar '<' >>. opt (pchar '=')
    |>> (fun x -> match x with None -> Less | _ -> LessOrEqual)

  let version: Parser<Version,unit> =
    let convertToUInt32 (txt :string) : Parser<_,_> =
      fun _ ->
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
    
    sepBy1 versionConstraint delimiter
    |>> (function [] -> failwith "can't be" | x::xs -> NEL.create x xs)

  let updateName =
    let letter = digit
             <|> asciiLetter
             <|> pchar '_'
    
    let toUpdateName = List.toArray
                    >> String
                    >> (function "Platform" -> Platform | n -> UpdateName n)
    
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
    sepBy dependency rf

type UpdateValidationError =
  | InvalidUpdate       of string
  | InvalidConstraints  of string
  | Other               of string

type VersionCheckResult =
  | CorrectVersion    of Update
  | AlreadyPublished  of Update
  | UnexpectedVersion of Update * Version

let checkUpdateVersion lookupSet target =
  match Seq.tryFind ((=) target) lookupSet with
  | Some upd ->
      AlreadyPublished upd

  | None ->
      match lookupSet
            |> Seq.filter (updName >> ((=) target.Name))
            |> Seq.map (fun u -> u.Version) 
            |> Seq.filter ((<) target.Version)
            |> Seq.sortDescending
            |> Seq.tryHead with
      | Some v ->
          UnexpectedVersion (target, v)
      
      | None ->
          CorrectVersion target

type ResolutionResult =
  | UpdateNotFound          of UpdateName * suggestions:UpdateName list
  | MissingUpdateVersion    of Activation
  | IncompatibleConstraints of Activation * Activation
  | Solution                of Tree<Update> list

type PublishingError =
  | MissingFileBody of Update

type AvailableResult =
  | ListAvailable of (Uri * Guid) list
  | UpdNotFound   of Update

type AcceptDownloadingResult =
  | AcceptingUpdNotFound  of Update
  | DownloadAccepted      of Update

type UpdateInfo = Update * UpdateSpecs

type CustomerId = string

type User =
  | Issuer of CustomerId


// type DomainMessage with

//   override dmsg.ToString () =
//     let doc =
//       match dmsg with
//       | Never -> Rep.Message.Zero |> Rep.pmsgToDoc
      
//       | ReadUpdateMessage msg ->
//           readUpdateResultToPrintMessage msg |> Rep.pmsgToDoc

//       | ResolutionMessage resmsg ->       
//           resolutionToPrintMessage resmsg |> Rep.pmsgToDoc

//       | VersionCheckMessage vcmsg ->
//           versionCheckToPrintMessage vcmsg |> Rep.pmsgToDoc

//       | AvailableMessage msg ->
//           availableToPrintMessage msg

//       | ConvertToEscMessage msg ->
//           convertionEscToPrintMessage msg |> Rep.pmsgToDoc
          
//       | PublishMessage msg ->
//           msg
//           |> publishResultToPrintMessage
//           |> Rep.pmsgToDoc

//       | AcceptDownloadingMessage msg ->
//           msg
//           |> acceptingDownloadToPrintMessage
//           |> Rep.pmsgToDoc

//       | UnexpectedError err ->
//           err |> PrettyPrint.PPrint.txt
    
//     in
//       doc |> PrettyPrint.PPrint.render ^ Some 78
