namespace MAptekaGet

[<AutoOpen>]
module Utils =
  let internal memoize (f: 'a -> 'b) : 'a -> 'b =
    let cache = System.Collections.Concurrent.ConcurrentDictionary<'a, 'b>()
    fun (x: 'a) ->
      cache.GetOrAdd(x, f)

  /// Memoize by 1st parameter value
  let internal memoize1 (f: 'a -> 'b -> 'c) : 'a -> 'b -> 'c =
    let cache = System.Collections.Concurrent.ConcurrentDictionary<'a, 'c>()
    fun (x: 'a) (y: 'b) ->
      cache.GetOrAdd(x, fun xi -> f xi y)


  let inline (<??>) x y = match x with Some _ -> x | None -> y

  module Parsing =
    open FParsec
    
    let inline rf<'a> : Parser<char option, unit> =
      opt (pchar '\r') .>> pchar '\n'

    /// applies the parser p, ignores the result, and returns x.
    let inline (>>%) p x = p |>> (fun _ -> x)

    let inline toResult res =
      match res with
      | Success (res,_,_) -> Result.Ok res
      | Failure _         -> Result.Error (sprintf "%A" res)
    
    let inline (<--) parser txt =
      run parser txt |> toResult