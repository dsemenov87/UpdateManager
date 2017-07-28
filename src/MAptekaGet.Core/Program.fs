namespace MAptekaGet

open System
open System.Threading

module Program =
  open DataAccess
  module UP = UpdaterProgram
  open Domain.Operations

  [<EntryPoint>]
  let main argv =
    let defaultConfig : App.Config =
      // default bind to 127.0.0.1:8080
      { IP = System.Net.IPAddress.Loopback
        Port = 8080us
        EscConvertUri = Uri "http://w7-grishin:1972/csp/updaptservice/User.UpdAptToEscService.cls"
        EscUriPrefix  = sprintf "http://test-mapteka-updater.itapteka.loc/esc/%O/%O"
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
        | invalidArgs ->
            printfn "error: invalid arguments %A" invalidArgs
            printfn "Usage:"
            printfn "    --ip ADDRESS   ip address (Default: %O)" config.IP
            printfn "    --port PORT    port (Default: %i)" config.Port
            exit 1
      in
        argv |> List.ofArray |> parseArgs defaultConfig
    
    
    // static objects...
    let mutable escStorage : Map<Update, EscFileInfo> = Map.empty

    // resolve dependecies here...
    let services : App.Services =
      { Db = InMemoryDbContext()
        EscRepository =
          { Get = fun k -> escStorage |> Map.tryFind k |> Ok 
            Put = fun k v -> escStorage <- Map.add k v escStorage; Ok (k, v)
          }
      }
    
    use cts = new CancellationTokenSource()

    let program =
      UP.choice
        [ UP.validateUpdate >>=
            (fun upds ->
                UP.readSpecs >>=
                  (fun specs ->
                      upds |> UP.checkVersion >>=
                        (fun _ ->
                            Seq.zip upds specs
                            UP.publish uis
                        )
                  )
                
                // |> UP.checkVersion
                // |> (fun _ -> UP.publish uis)
            )
          
          UP.authorize >>= UP.availableUpdates

          UP.validateInput >>= (fun (maybeUser, uis) ->
            UP.prepareToInstall maybeUser (List.map fst uis)
          )
        ]

    App.interpretProgram config services cts program;

    printfn "Make requests now"
    Console.ReadKey true |> ignore
        
    cts.Cancel()

    0

