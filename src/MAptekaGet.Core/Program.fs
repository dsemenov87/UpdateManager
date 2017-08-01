namespace MAptekaGet

open System
open System.Threading

module Program =
  open DataAccess
  module UP = UpdaterProgram
  open Domain.Operations
  open Suave.Logging

  let env = Environment.GetEnvironmentVariable

  [<EntryPoint>]
  let main argv =
    // "ESC_CONVERT_URI" "http://w7-grishin:1972/csp/updaptservice/User.UpdAptToEscService.cls"
    // "ESC_URI_PREFIX" "http://test-mapteka-updater.itapteka.loc/esc/"    

    let defaultConfig : App.Config =
      // default bind to 127.0.0.1:8080
      { IP = System.Net.IPAddress.Loopback
        Port = 8080us
        EscConvertUri = Uri (env "ESC_CONVERT_URI")
        EscUriPrefix  = Uri (env "ESC_URI_PREFIX")
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
        | "--esc-convert-uri" :: escUriPrefix :: xs -> parseArgs { config with EscConvertUri = Uri escUriPrefix } xs
        | "--esc-uri-prefix" :: escUriPrefix :: xs -> parseArgs { config with EscUriPrefix = Uri escUriPrefix } xs
        | invalidArgs ->
            printfn "error: invalid arguments %A" invalidArgs
            printfn "Usage:"
            printfn "    --ip               ADDRESS ip address (Default: %O)" defaultConfig.IP
            printfn "    --port             PORT    port (Default: %i)" defaultConfig.Port
            printfn "    --esc-convert-uri  URI     uri (Default: %O)" defaultConfig.EscConvertUri
            printfn "    --esc-uri-prefix   URI     uri (Default: %O)" defaultConfig.EscUriPrefix
            exit 1
      in
        argv |> List.ofArray |> parseArgs defaultConfig

    // static objects...
    let mutable escStorage : Map<Update, EscFileInfo> = Map.empty

    // resolve dependecies here...
    
    let services : App.Services =
      { Db = InMemoryDbContext(env "UPD_BASE_URI") //"http://test-mapteka-updater.itapteka.loc/upd/"
        EscRepository =
          { Get = fun k -> escStorage |> Map.tryFind k |> Ok 
            Put = fun k v -> escStorage <- Map.add k v escStorage; Ok (k, v)
          }
      }
    
    use cts = new CancellationTokenSource()

    let program =
      UP.authorize >>= (fun user ->
        UP.choice
          [ UP.validateUpdate >>= (fun upd ->
              UP.readSpecs >>= (fun specs ->
                upd |> UP.checkVersion >>= (fun _ -> UP.publish (upd, specs) >>= UP.ignore)
              )
            )
            
            UP.availableUpdates user >>= UP.ignore

            UP.readUpdateAndUser >>= (fun (upd, user) ->
              UP.convertToEsc user upd >>= (fun _ ->
                UP.prepareToInstall user upd
              )
            )
          ]
      )

    App.interpretProgram config services cts program;

    printfn "Make requests now"
    Console.ReadKey true |> ignore
        
    cts.Cancel()

    0

