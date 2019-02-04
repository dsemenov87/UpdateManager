module Application

open System
open DomainServices

// TODO persistent storage
module Db = DataAccess.InMemory

module Http =
    open Suave
    open Suave.Successful
    open Suave.Operators
    open Suave.Filters
    open Suave.Utils
    open Suave.RequestErrors
    open Suave.Logging

    let inline fieldOrEmpty name (req: HttpRequest) =
      match req.fieldData name with Choice1Of2 name_ -> name_ | _ -> ""

    let inline externalHost scheme (req: HttpRequest) =
      sprintf "%s://%s" scheme req.clientHostTrustProxy

    let authorize next =
      request (fun req ->
        match req.header "X-User" with
        | Right user  -> next user
        | _           -> UNAUTHORIZED "You should set 'X-User' header." 
      )

    let readUpdateSpecs req =
        { Author        = req |> fieldOrEmpty "Author" 
          Summary       = req |> fieldOrEmpty "Summary"
          UniqueCode    = req |> fieldOrEmpty "UniqueCode"
          Description   = req |> fieldOrEmpty "Description"
          ReleaseNotes  = "" // todo
          Created       = DateTime.UtcNow
        }

    let readConstraints =
      fieldOrEmpty "Constraints"
      >> (fun c -> c.Split('\n') |> Seq.toList)


    // let showAvailableUpdates reqToBody user externalHost next =
    //   request (fun req ->
    //     let (Issuer customerId) = user
    //     let externalUri = req |> externalHost |> sprintf "%s/api/v1/" |> Uri
    //     in
    //       customerId
    //       |> availableUpdates db externalUri srv.EscRepository (reqToBody req)
    //       |> keepGoingIfSucceedAsync next
    //   )

    let routing =
      path "/api/v1"
      >=> choose
        [ path "auth" >=> OK "StubToken" // stub for test

          pathScan "updates/%s/%s" (fun (name, vers) ->
              authorize (fun _ ->
                POST >=>
                    request (fun req ctx ->
                      req
                      |> readConstraints
                      |> validateUpdateAndConstrs name vers
                      |> Choice.bindAsync (checkVersion Db.getVersionsByName)
                      |> Async.bind ^ Choice.bindAsync (fun upd ->
                          publishUpdate Db.upsert upd (readUpdateSpecs req) ["path/to/tmp_file"] // TODO file upload
                      )
                      |> Async.bind (function
                        | Right success -> OK success ctx
                        | Left problem  -> BAD_REQUEST problem ctx)
                    )
              )
            )
        ]