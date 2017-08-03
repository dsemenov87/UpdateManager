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

    let defaultConfig : App.Config =
      // default bind to 127.0.0.1:8080
      { IP = System.Net.IPAddress.Loopback
        Port = 8080us
        EscConvertUri = Uri (env "ESC_CONVERT_URI"  |> Choice.orDefault "http://w7-grishin:1972/csp/updaptservice/User.UpdAptToEscService.cls")
        EscUriPrefix  = Uri (env "ESC_URI_PREFIX"   |> Choice.orDefault "http://test-mapteka-updater.itapteka.loc/esc/")
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
    let mutable escStorage : Map<CustomerId, (EscFileInfo * Update Set * bool) seq> = Map.empty

    // resolve dependecies here...
    
    let services : App.Services =
      { Db = InMemoryDbContext(env "UPD_BASE_URI" |> Choice.orDefault "http://test-mapteka-updater.itapteka.loc/upd/")
        EscRepository =
          { Get = fun cid -> escStorage |> Map.tryFind cid |> (function None -> Ok Seq.empty | Some x -> Ok x)
            Put = fun cid efi updSet fetched ->
              let newValue =
                let entry = [(efi, updSet, fetched)]
                match Map.tryFind cid escStorage with
                | None      -> seq entry
                | Some ecss -> Seq.append entry ecss
              
              escStorage <- Map.add cid newValue escStorage; Ok efi
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

    App.interpretProgram config services cts program |> ignore

    0

