namespace MAptekaGet

[<AutoOpen>]
module Utils =
  module Parsing =
    open FParsec
    
    let inline rf<'a> : Parser<char option, unit> =
      opt (pchar '\r') .>> pchar '\n' <?> "end of line"

    /// applies the parser p, ignores the result, and returns x.
    let inline (>>%) p x = p |>> (fun _ -> x)

    let inline toResult res =
      match res with
      | Success (res,_,_) -> Result.Ok res
      | Failure _         -> Result.Error (sprintf "%A" res)
    
    let inline (<--) parser txt =
      run parser txt |> toResult