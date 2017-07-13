namespace MAptekaGet

open System
open DataAccess
open Domain.Operations

module UP = UpdaterProgram
module Rep = Reporting

type IUpdateManager =
  abstract member Check: IO.Stream -> Net.HttpStatusCode * string
  abstract member Publish: IO.Stream * UpdateFileInfo -> Net.HttpStatusCode * string
  // abstract member Install: Update * Version -> UpdateDetails * string

[<AutoOpen>]
module UpdateManagerUtils =
  open Parsing
  open Utils.Parsing
  open ResultOp
  open System.Xml.Linq
  
  module NEL = NonEmptyList
  module DR = DependencyResolution

  let rec private addConstraintRes<'err> (upd: Result<Update, 'err>) (constrs: Constraint list) =
    match constrs with
    | []          -> upd
    | constr::cs  -> addConstraintRes (upd <!> addConstraint constr) cs

  let xElements (name: string) (xmlReader: Xml.XmlReader) =
    seq {
      while xmlReader.ReadToFollowing name do
        match XElement.ReadFrom xmlReader with
        | :? XElement as el -> yield el
        | _ -> ()
    }

  let (|Name|Version|Author|Summary|Description|Constraint|Other|)
    (xName: XAttribute, xValue: XAttribute) =
    
    match xName.Value with
    | "Name"         -> Name xValue.Value
    | "Version"      -> Version xValue.Value
    | "Author"       -> Author xValue.Value
    | "Summary"      -> Summary xValue.Value
    | "Description"  -> Description xValue.Value
    | "Constraint"   -> Constraint xValue.Value
    | _              -> Other xValue.Value
  
  type private UpdateAccumulator =
    { Name        : string
      Version     : string
      Constraints : string list
      Author      : string
      Summary     : string
      Description : string
    } 

    static member Zero =
      { Name          = ""
        Version       = ""
        Constraints   = []
        Author        = ""
        Summary       = ""
        Description   = ""
      }

  let private parseUpdateSpecs (xml: IO.Stream) =
    try
      use reader = Xml.XmlReader.Create xml
      reader
      |> xElements "entry" 
      |> Seq.collect (fun el ->
        el.Attributes (XName.Get "name")
        |> Seq.collect (fun xName ->
          el.Attributes (XName.Get "value")
          |> Seq.map (fun xValue -> xName, xValue)
        )
      )
      |> Seq.fold (fun accRes attrs ->
          accRes
          >>= (fun acc ->
              match attrs with
              | Name name ->
                  if String.IsNullOrWhiteSpace acc.Name then
                    Ok { acc with Name = name }
                  else
                    Error "Duplicate 'name' entry in specs."
              
              | Version v ->
                  Ok { acc with Version = v }
              | Constraint c ->
                  Ok { acc with Constraints = c :: acc.Constraints }
              | Author a ->
                  Ok { acc with Author = a }
              | Summary s ->
                  Ok { acc with Summary = s }
              | Description d ->
                  Ok { acc with Description = d }
              | Other problem ->
                  Error problem
        )
      ) (Ok UpdateAccumulator.Zero)
      
    with
      | :? System.Xml.XmlException as ex ->
        Error (show ex)

  let private validateUpdate (acc: UpdateAccumulator) =  
    acc.Constraints
    |> List.map ((<--) dependency)
    |> ResultExt.sequence
    >>= addConstraintRes (update <-- acc.Name)
    |> (function
      | Ok upd    ->
          Valid (upd, { Author        = acc.Author
                        Summary       = acc.Summary
                        Description   = acc.Description
                        ReleaseNotes  = "" // todo
                        Created       = DateTime.UtcNow
                      })
      | Error err -> Invalid err
    )

  let private checkVersion (db: IDbContext) (upd: Update, updspecs: UpdateSpecs) =
    db.GetVersionsByName upd.Name
    <!> List.map (fun u -> u.Version)
    <!> (fun vers ->
      match vers
            |> List.filter ((>=) upd.Version)
            |> List.sortDescending
            |> List.tryHead with
      | Some v when upd.Version > v -> Unexpected (upd, v)
      | Some v                      -> AlreadyPublished upd
      | None                        -> CorrectVersion (upd, updspecs)
    )
    
  let private resolveDependencies (db: IDbContext) (updates: Update list) =
    match updates with
    | [] -> Solution [] |> Ok
    | u::us ->
        updates
        |> db.GetDependencies   
        <!> DR.resolve
        <*> Ok (NEL.create u us)

  type internal HttpInterpreterState =
    { HttpStatusCode: System.Net.HttpStatusCode
      Message       : Rep.Message list
    }

    static member Zero
      with get () = { HttpStatusCode  = System.Net.HttpStatusCode.OK 
                      Message         = [Rep.Message.Zero]
                    }

  
  let interpretAsHttpUpdater (db: IDbContext) (program: UpdaterProgram<'a>) =
    
    let toErrorMessage state err =
      (System.Net.HttpStatusCode.InternalServerError, show (Rep.Message.New err [] Rep.MessageMode.Error))

    let toBadRequest state msg =
      (System.Net.HttpStatusCode.BadRequest, show msg)
    
    let rec recurse (state: HttpInterpreterState) program : System.Net.HttpStatusCode * string =
      match program with
      | Stop a ->
          (state.HttpStatusCode, show state.Message)

      | KeepGoing (ValidateInput (xml, next)) ->
          let res =
            xml
            |> parseUpdateSpecs
            <!> validateUpdate

          match res with
          | Ok ((Valid (upd, updspecs)) as valid) ->
              let state = {state with Message = [Rep.validationToMsg valid] }
              recurse state (next (upd, updspecs))
          | Ok invalid ->
              toBadRequest state (Rep.validationToMsg invalid)  
              
          | Error err ->
              toErrorMessage state err

      | KeepGoing (CheckVersion (upd, next)) ->
          let res =
            upd
            |> checkVersion db

          match res with
          | Ok ((CorrectVersion (upd, updspecs)) as r) ->
              let state = {state with Message = [Rep.versionCheckToMsg r]}
              recurse state (next (upd, updspecs))
          | Ok r ->
              toBadRequest state (Rep.versionCheckToMsg r)
          | Error err ->
              toErrorMessage state err

      | KeepGoing (ResolveDependencies (upds, next)) ->
          let res =
            upds
            |> resolveDependencies db
          
          match res with
          | Ok (Solution acts) ->
              let state = {state with Message = [Rep.resolutionToMsg (Solution acts)]}
              recurse state (next acts)
          | Ok r ->
              toBadRequest state (Rep.resolutionToMsg r)
          | Error err ->
              toErrorMessage state err       

      | KeepGoing (Publish ((upd, updspecs, fi), next)) -> 
          match db.Upsert (upd, updspecs, fi) with
          | Ok upd ->
              let msg = Rep.Message.New (sprintf "Update %O was published." upd) [] Rep.MessageMode.Success
              let state = {state with Message = msg :: state.Message}
              recurse state (next upd)
          | Error err ->
              toErrorMessage state err   

      | _ ->
          failwith "not implemented yet"

    in
      recurse HttpInterpreterState.Zero program

type UpdateManager (db: IDbContext) =
  let check specs =
    UP.validateInput specs
    >>= UP.checkVersion

  interface IUpdateManager with
    member x.Check (specs: IO.Stream) =
      let program =
        specs |> check >>= Stop
      in
        interpretAsHttpUpdater db program

    member x.Publish (xmlspecs: IO.Stream, ufi: UpdateFileInfo) =
      let program =
        xmlspecs |> check >>= (UP.publish ufi) >>= Stop
      in
        interpretAsHttpUpdater db program
      
