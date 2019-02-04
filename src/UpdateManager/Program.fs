namespace MAptekaGet

open System
open FsConfig

type Config =
  { 
    [<DefaultValue("localhost")>]
    IP            : Net.IPAddress

    [<DefaultValue("8080")>]
    Port          : uint16

    StaticBaseUri : Uri

    [<DefaultValue("server=localhost;port=5432;database=updater")>]
    DbConnectionString : string

    [<DefaultValue("Info")>]
    logLevel      : Suave.Logging.LogLevel
  }

module Program =
  open Suave
  open Suave.Operators
  open Suave.Filters

  [<EntryPoint>]
  let main argv =

    let parse f (txt : string) = match f txt with (true, i) -> Some i | _ -> None

    let (|Port|_|) = parse System.UInt16.TryParse
    let (|IPAddress|_|) = parse System.Net.IPAddress.TryParse

    let rec parseArgs (config: Config) args =
      match args with
      | [] -> config
      | "--ip" :: IPAddress ip :: xs -> parseArgs { config with IP = ip } xs
      | "--port" :: Port p :: xs -> parseArgs { config with Port = p } xs
      | invalidArgs ->
          printfn "error: invalid arguments %A" invalidArgs
          printfn "Usage:"
          printfn "    --ip               ADDRESS ip address (Default: %O)" config.IP
          printfn "    --port             PORT    port (Default: %i)" config.Port
          exit 1

   // parse arguments
    let config =
      match EnvConfig.Get<Config>() with
      | Ok config ->
          parseArgs config (List.ofArray argv)

      | Error (NotFound envVarName) ->
          failwithf "Environment variable %s not found" envVarName

      | Error (BadValue (envVarName, value)) ->
          failwithf "Environment variable %s has invalid value %s" envVarName value

      | Error (NotSupported msg) ->
           failwith msg

    // resolve dependecies here...
    
    let logger =
      Suave.Logging.LiterateConsoleTarget([||], config.logLevel) :> Suave.Logging.Logger
    
    let serverLogging =
        logWithLevelStructured Logging.Debug logger (fun ctx ->
          let fields =
            [ "requestMethod"       , box ctx.request.method
              "requestPathAndQuery" , box ctx.request.url.PathAndQuery
              "requestId"           , box ctx.request.trace.traceId
              "httpStatusReason"    , box ctx.response.status.reason
              "httpStatusCode"      , box ctx.response.status.code
              "requestForm"         , box ctx.request.form
            ]
            |> Map.ofList

          let format = 
            "HTTP {requestMethod} at \"{requestPathAndQuery}\" responded {httpStatusReason} ({httpStatusCode})"
          in
            (format, fields)
      )
    
    let serverConfig =
      { defaultConfig
          with  bindings = [ HttpBinding.create HTTP config.IP config.Port ]
                logger = logger
      }

    startWebServer serverConfig (Application.Http.routing >=> serverLogging)

    0

