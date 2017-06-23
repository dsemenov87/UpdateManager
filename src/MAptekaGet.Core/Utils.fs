module internal MAptekaGet.Utils

let internal memoize (f: 'a -> 'b) : 'a -> 'b =
  let cache = System.Collections.Concurrent.ConcurrentDictionary<'a, 'b>()
  fun (x: 'a) ->
    cache.GetOrAdd(x, f)

let inline (++) x y = match x with None -> y | _ -> x

module Parsing =
  open FParsec

  /// applies the parser p, ignores the result, and returns x.
  let (>>%) p x = p |>> (fun _ -> x)

  let toResult text parser =
    let res = run parser text
    match res with
    | Success (res,_,_) -> Result.Ok res
    | Failure _         -> Result.Error (sprintf "%A" res)