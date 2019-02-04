module DomainServices

open FSharp.Control

open ChoiceInfixes
open Parsing
open Reporting

type CheckIfUpdateExists = Update -> Async<Choice<Option<Update * UpdateSpecs>, string>>

type GetAvailableUpdates = CustomerId -> Async<Choice<Update Set, string>>

type AddUpdatesByUniqueCode = CustomerId -> string list -> Async<Choice<unit, string>>

type GetVersionsByName = UpdateName -> Async<Update Set>

type UpsertUpdate = Update -> UpdateSpecs -> string -> Async<Choice<Update, string>>

type AddToUsers = Map<Update, CustomerId> -> Async<Choice<unit, string>>

type NearbyNames = UpdateName -> Async<Choice<UpdateName seq, string>>

let rec private addConstraintRes<'err> (upd: Choice<Update, 'err>) (constrs: Constraint list) =
  match constrs with
  | []          -> upd
  | constr::cs  -> addConstraintRes (upd <!> addConstraint constr) cs

let private validateUpdate name vers =
  update <-- (name + "-" + vers)

// let private resolveDependencies (db: UpdRepository) (logger: Logger) (updates: Update seq) =
//   async {
//     let dmsg = ResolutionMessage
//     match Seq.toList updates with
//     | [] ->
//       return Right ([], dmsg (Solution []))

//     | u::us ->
//       let! res = DR.resolve db.GetVersionsByName (NEL.create u us)

//       logger.debug (
//         eventX "Dependencies of {updates}: {deps}"
//           >> setField "updates" (sprintf "%A" updates)
//           >> setField "deps" (sprintf "%A" res))

//       match res with
//       | Solution forest as sol ->
//           return Right (forest, dmsg sol)

//       | UpdateNotFound (updName, _) ->
//           let! nearbyNames = db.NearbyNames updName
//           match nearbyNames with
//           | Right ns -> return Left ^ dmsg ^ UpdateNotFound (updName, Seq.toList ns)
//           | Left err -> return Left ^ UnexpectedError err
      
//       | res ->
//           return Left (dmsg res)
//   }

// let private checkUserUpdates (db: UpdRepository) (rawList: string list) =
//   rawList
//   |> Set.ofList
//   |> Seq.map (fun txt ->
//     (update <-- txt)
//     <!> db.HeadUpdate
//     <!> Async.map (Choice.bind ^ Choice.ofOption (sprintf "Update '%s' not found" txt))
//     |> Choice.revAsync
//     |> Async.map (Choice.bind id)
//   )
//   |> Async.Parallel
//   |> Async.map (
//       Seq.toList
//       >> Choice.sequence
//       >> Choice.map (Seq.map fst >> Set.ofSeq)
//       >> Choice.mapSnd InvalidUpdate
//   )

// let private getInstalledUpdatesFromProtocol (db: UpdRepository) =
//   IOHelpers.extractProtocolFromRequest
//   >> Async.map (IOHelpers.extractUniqueCodesFromProtocol >> Seq.toList)

// let private availableUpdates (db: UpdRepository) externalUri (escRepository: EscRepository) (compressedProtocol: byte[] option) userId =
//   let getFromDb () =
//     escRepository.Head userId
//     |> Async.map ^ Choice.mapSnd ^ UnexpectedError
//     |> Async.map ^ Choice.bind (
//         Seq.map (fun (eid, (upds,_)) ->
//           let link = escRepository.GetDownloadLink eid externalUri
//           ((link, eid), upds)
//         )
//         >> Seq.toList
//         >> (fun lst -> Right (lst, lst |> List.map fst |> ListAvailable |> AvailableMessage))
//     )
//   in
//     compressedProtocol
//     |> Option.map (
//       getInstalledUpdatesFromProtocol db
//       >> Async.bind (db.AddUpdatesByUniqueCode userId)
//       >> Async.bind (ignore >> getFromDb)
//     )
//     |> Option.defaultValue (getFromDb ())

// let private prepareToInstall (user: CustomerId) (db: UpdRepository) (rawList: string list) =
//   checkUserUpdates db rawList
//   |> Async.bind ^ Choice.bindAsync (fun upds ->
//     user
//     |> List.replicate (Seq.length upds)
//     |> Seq.zip upds
//     |> Map.ofSeq
//     |> db.AddToUsers
//     |> Async.map ^ Choice.mapSnd Other
//   )

// let private acceptDownloading (escRepository: EscRepository) (eid: EscId) user =
//   user
//   |> escRepository.Head
//   |> Async.map ^ Choice.mapSnd UnexpectedError
//   |> Async.bind ^ Choice.bindAsync (
//       Seq.filter (fst >> ((=) eid))
//       >> Seq.collect (fun (_,(upds,_)) -> upds)
//       >> Set.ofSeq
//       >> (fun upds ->
//         if Set.isEmpty upds then
//           eid
//           |> AcceptingEscNotFound 
//           |> AcceptDownloadingMessage
//           |> Left
//           |> Async.result
//         else
//           escRepository.AcceptDownloading eid
//           |> Async.map ^ Choice.mapSnd UnexpectedError
//           |> Async.map ^ Choice.map (fun () -> (), eid |> DownloadAccepted |> AcceptDownloadingMessage)
//       )
//   )

let publishUpdate (upsertFn : UpsertUpdate) upd specs = 
  List.tryHead
  >> Option.toChoice (upd |> MissingFileBody |> showPublishError)
  >> Choice.bindAsync (fun filePath ->
      upsertFn upd specs filePath
      |> Async.map ^ Choice.map showSuccessPublishing
  )

let validateUpdateAndConstrs name vers constrs =  
  constrs
  |> List.map ((<--) dependency)
  |> Choice.sequence
  <?> InvalidConstraints
  >>= addConstraintRes (validateUpdate name vers <?> InvalidUpdate)
  <?> showUpdateValidationError

let checkVersion (fetch: GetVersionsByName) (upd: Update) =
  upd.Name
  |> fetch
  |> Async.map (fun upds -> checkUpdateVersion upds upd)
  |> Async.map (function
    | CorrectVersion x  -> Right upd
    | problem           -> Left (showVersionCheckResult problem)
  )
