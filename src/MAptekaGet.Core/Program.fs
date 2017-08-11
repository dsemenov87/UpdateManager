namespace MAptekaGet

open System
open System.Threading

open Suave.Utils

module Program =
  open DataAccess
  open Domain.Operations
  open Parsing

  module UP = UpdaterProgram

  // [<EntryPoint>]
  // let main _ =

    // let newUpdate name major minor patch =
    //   { Name        = UpdateName name
    //     Version     = {Major=major; Minor=minor; Patch=patch;}
    //     Constraints = []
    //   }

    // let ``common_nskPricingCheck``num =
    //   newUpdate "common_nskPricingCheck" 0u num 0u

    // let target =
    //   ``common_nskPricingCheck`` 33u

    // [ ``common_nskPricingCheck`` 34u ]
    // |> Seq.map (fun u -> u.Version) 
    // |> Seq.filter ((<=) target.Version)
    // |> Seq.sortDescending
    // |> Seq.tryHead
    // |> (fun x -> target.Version > v ->
    //     UnexpectedVersion (upd, v)
    
    // | Some v ->
    //     AlreadyPublished upd
    
    // | None ->
    //     CorrectVersion upd

    // 0
  
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
        let! depsTree = UP.resolveDependencies updates
        let deps = depsTree |> Seq.collect Tree.toSeq |> Seq.distinct
        do! UP.convertToEsc target deps >>= UP.ignore
        do! UP.prepareToInstall target updates >>= UP.ignore
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
    
    let program1 =
      UP.authorize >>= (fun user ->
        UP.choose
          [ UP.readUpdate >>= (fun upd ->
              UP.readSpecs >>= (fun specs ->
                upd
                |> UP.checkVersion
                >>= (Set.singleton >> UP.resolveDependencies)
                >>= (fun _ -> UP.publish (upd, specs) >>= UP.ignore)
              )
            )
            
            UP.availableUpdates user >>= UP.ignore

            UP.readUserUpdates >>= (fun (updSet, user) ->
              updSet |> UP.resolveDependencies >>= (
                Seq.collect Tree.toSeq
                >> Set.ofSeq
                >> UP.convertToEsc user
                >=> (fun _ ->
                  UP.prepareToInstall user updSet
                )
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

