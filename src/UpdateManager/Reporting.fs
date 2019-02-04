module Reporting

open System
open PrettyPrint
open Suave.Utils

module internal Json =
    open Chiron

    let updInfoToJson (uri, md5 : Guid) =
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

let inline showNode x y = sprintf "%O => %O" x y

type Message =
  { Summary : string
    Details : Doc list
  }

  static member New (s: string) = fun d ->
    { Summary = s; Details = d }

  static member Zero
    with get () =
      Message.New "" []

type IToReportMessage =
  abstract member ToReportMessage: unit -> Message

/// Neat 2-dimensional drawing of a tree.
let rec drawTree (Node (s, ts): Tree<string>) : Doc =
  let shift first other br =
    chr '|' <.> ((txt first <+> drawTree br) |> nestBy other)
  let rec drawBranches = function
    | []      -> Doc.Empty
    | [br]    -> shift "`-" "   " br
    | br::brs -> shift "+-" "|  " br <.> (drawBranches brs) 
  in
    txt s <.> (ts |> Seq.toList |> drawBranches)
let private verticalAppend (a: Doc) (b: Doc) : Doc =
  a <^> line <^> line <^> b

let private stack (allDocs: Doc list) : Doc =
  match allDocs with
  | [] ->
      failwith "Do not use `stack` on empty lists."
  
  | doc::docs ->
      List.fold verticalAppend doc docs
 
let private start mode : Doc =
  softbreak <^> (mode |> string |> txt) <^> txt ":"

let internal pmsgToDoc (pmsg: Message) : Doc =
  let {Summary=summary;Details=details} = pmsg
  let allDocs =
    if System.String.IsNullOrWhiteSpace summary then
      details
    else
      let summaryDoc =
        fillSep (summary |> words |> List.map txt)
      in
        summaryDoc :: (details |> List.map (indent 4))
  in  
    stack allDocs
    <^> line
    <^> line

let inline choiceToMsg res =
  let msg =
    match res with
    | Choice1Of2 res -> string res
    | Choice2Of2 err -> string err
  in
    Message.New "" [txt msg]

type Message with
  override x.ToString() =
    x|> pmsgToDoc |> render ^ Some 78

let rec internal treeFromActivation (act: Activation) =
  
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
    | InitialA upd -> NEL.singleton (string upd)
    | ChildA (upd, lastConstr, constrAct) ->
        NEL.create (string upd) (Seq.toList (showUpToRoot lastConstr constrAct))
        |> NEL.reverse
    |> Tree.fromNonEmptyList

let showUpdateValidationError (msg: UpdateValidationError) =
  match msg with
  | InvalidUpdate problem ->
      Message.New
        ( "Invalid update:"
        )
        [ txt problem ]
        |> string

  | InvalidConstraints problem ->
      Message.New
        ( "Constraints format is invalid:"
        )
        [ txt problem ]
        |> string
  
  | Other problem ->
      Message.New
        ( "Unknown problem."
        )
        [txt problem]
        |> string

let showPublishError (err: PublishingError) =
  match err with
  | MissingFileBody upd ->
      Message.New
        ( sprintf "File body for update '%O' is missed." upd
        )
        []
        |> string

let showSuccessPublishing (upd: Update) =
    Message.New
      ( sprintf "Update '%O' is published." upd
      )
      []
      |> string
  

let showAvailableResult (msg: AvailableResult) =
  match msg with
  | ListAvailable upds ->
      upds
      |> List.map Json.updInfoToJson
      |> Json.jsonMessagesToSingleJson
      |> PrettyPrint.Json.pretty
      |> render ^ Some 78

  | UpdNotFound upd ->
      Message.New
        ( "Cannot find *.esc file for update '" + string upd + "'."
        )
        []
        |> string

let showResolutionResult (msg: ResolutionResult) =
  match msg with
  | Solution forest ->
      Message.New
        ( "The update dependencies:"
        )
        ( forest |> List.map (Tree.map string >> drawTree))
      

  | UpdateNotFound (updName, suggestions) ->
      Message.New
        ( "Could not find any update named '" + string updName + "'. Maybe you want one of those?"
        )
        [ List.map (txt << string) suggestions |> vcat
        ]

  | IncompatibleConstraints (act1, act2) ->
      Message.New
        ( "There was a conflict for update version:")
        [ drawTree (treeFromActivation act1)
          drawTree (treeFromActivation act2) 
        ]

  | MissingUpdateVersion act ->
      let docTree, update =
        let update = act |> activationUpdate |> string
        match act with
        | InitialA _ -> txt update, update 
        | ChildA (_, constr, parentAct) ->
            let tree =
              Node (showNode (activationUpdate parentAct) constr, seq [update |> string |> Tree.singleton])
              // |> Tree.map (activationUpdate >> string)
            in
              drawTree tree, update

      Message.New
        ( "Cannot resolve dependencies. Missing update version."
        )
        [ docTree ]

let showVersionCheckResult (msg: VersionCheckResult) =
  match msg with
  | CorrectVersion upd ->
      Message.New
        ( "Update version '" + (string upd) + "' is correct."
        )
        []
        |> string
  
  | AlreadyPublished upd ->
      Message.New
        ( "Update '" + (string upd) + "' has already been published. You cannot publish it again!"
        )
        []
        |> string

  | UnexpectedVersion (_, version) ->
      Message.New
        ( "The next version should be greater then '" + (string version) + "'."
        )
        []
        |> string

let showAcceptDownloadingResult (msg: AcceptDownloadingResult) =
  match msg with
  | AcceptingUpdNotFound upd ->
      Message.New
        ( sprintf "The accepting '%O.upd' file is not found." upd
        )
        []
        |> string

  | DownloadAccepted upd ->
      Message.New
        ( sprintf "'%O'.upd file downloading is accepted." upd
        )
        []
        |> string
