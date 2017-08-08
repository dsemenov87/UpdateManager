namespace MAptekaGet

open System
open System.Threading

open Suave.Utils

module Program =
  open DataAccess
  open Domain.Operations

  module UP = UpdaterProgram

  [<EntryPoint>]
  let main argv =

    let staticBaseUri =
      Uri (env "STATIC_BASE_URI"  |> Choice.orDefault "http://localhost/") //"http://test-mapteka-updater.itapteka.loc/"

    let escConvertUri =
      Uri (env "ESC_CONVERT_URI"  |> Choice.orDefault "http://localhost:1972/csp/updaptservice/User.UpdAptToEscService.cls")

    let escExternalScheme =
      env "ESC_EXT_SCHEME" |> Choice.orDefault "http"
    
    let defaultConfig : App.Config =
      // default bind to 127.0.0.1:8080
      { IP = System.Net.IPAddress.Loopback
        Port = 8080us
        StaticBaseUri = staticBaseUri
        EscConvertUri = escConvertUri
        EscExternalScheme = escExternalScheme
      }
    
    // parse arguments
    let config =
      let parse f str = match f str with (true, i) -> Some i | _ -> None

      let (|Port|_|) = parse System.UInt16.TryParse
      let (|IPAddress|_|) = parse System.Net.IPAddress.TryParse

      let rec parseArgs (config: App.Config) args =
        match args with
        | [] -> config
        | "--ip" :: IPAddress ip :: xs -> parseArgs { config with IP = ip } xs
        | "--port" :: Port p :: xs -> parseArgs { config with Port = p } xs
        | "--esc-ext-sheme" :: escExternalScheme :: xs -> parseArgs { config with EscExternalScheme = escExternalScheme } xs
        | invalidArgs ->
            printfn "error: invalid arguments %A" invalidArgs
            printfn "Usage:"
            printfn "    --ip               ADDRESS ip address (Default: %O)" defaultConfig.IP
            printfn "    --port             PORT    port (Default: %i)" defaultConfig.Port
            printfn "    --esc-convert-uri  URI     uri (Default: %O)" defaultConfig.EscConvertUri
            exit 1
      in
        argv |> List.ofArray |> parseArgs defaultConfig

    // resolve dependecies here...
    
    let services : App.Services =
      { UpdRepository = inMemoryUpdRepository staticBaseUri 
        EscRepository = inMemoryEscRepository staticBaseUri
      }

    let program =
      UP.authorize >>= (fun user ->
        UP.choice
          [ UP.validateUpdate >>= (fun upd ->
              UP.readSpecs >>= (fun specs ->
                upd |> UP.checkVersion >>= (fun _ -> UP.publish (upd, specs) >>= UP.ignore)
              )
            )
            
            UP.availableUpdates user >>= UP.ignore

            UP.readUserUpdates >>= (fun (upds, user) ->
              UP.convertToEsc user upds >>= (fun _ ->
                UP.prepareToInstall user upds
              )
            )

            UP.readEscUri >>= (fun escUri ->
              let (Issuer customerId) = user
              in
                UP.acceptDownloading customerId escUri
            )
          ]
      )

    App.interpretProgram config services program |> ignore

    0

