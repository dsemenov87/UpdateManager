namespace MAptekaGet

open System
open System.Threading

open Suave.Utils

module Program =
  open DataAccess
  open DataAccessSql
  open Domain.Operations
  open Parsing

  module UP = UpdaterProgram

  [<EntryPoint>]
  let main argv =

    let staticBaseUri =
      Uri (env "STATIC_BASE_URI"  |> Choice.orDefault "http://test-mapteka-updater.itapteka.loc/")

    let escConvertUri =
      Uri (env "ESC_CONVERT_URI"  |> Choice.orDefault "http://localhost:1972/csp/updaptservice/User.UpdAptToEscService.cls")

    let escExternalScheme =
      env "ESC_EXT_SCHEME" |> Choice.orDefault "http"

    let dbConnStr =
      env "DB_CONNECTION_STR" |> Choice.orDefault "server=localhost;port=5432;database=mapteka_get;user id=postgres;password=123"
    
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
      { UpdRepository = sqlUpdRepository dbConnStr staticBaseUri //inMemoryUpdRepository staticBaseUri 
        EscRepository = sqlEscRepository dbConnStr staticBaseUri //inMemoryEscRepository staticBaseUri
      }

    let program = updater {
      let! user = UP.authorize

      let publish = updater {
        let! newUpdate = UP.readUpdate
        let! specs = UP.readSpecs
        let! newUpdate = UP.checkVersion newUpdate
        do! UP.resolveDependencies (Set.singleton newUpdate) >>= UP.ignore
        do! UP.publish (newUpdate, specs) >>= UP.ignore
      }

      let listAvailable = UP.availableUpdates user >>= UP.ignore

      let prepareToUnstall = updater {
        let! (updates, target) = UP.readUserUpdates
        do! UP.prepareToInstall target updates >>= UP.ignore
        
        let! depsTree = UP.resolveDependencies updates
        let deps = depsTree |> Seq.collect Tree.toSeq |> Seq.distinct
        do! UP.convertToEsc target deps >>= UP.ignore
      }

      let acceptDownloading = updater {
        let! escUri = UP.readEscUri
        let (Issuer customerId) = user
        do! UP.acceptDownloading customerId escUri
      }

      do! UP.choose
            [ publish
              listAvailable
              prepareToUnstall
              acceptDownloading
            ]
    }
    
    App.interpretProgram config services program |> ignore

    0

