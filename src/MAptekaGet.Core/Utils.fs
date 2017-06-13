module internal MAptekaGet.Utils

let internal memoize (f: 'a -> 'b) : 'a -> 'b =
  let cache = System.Collections.Concurrent.ConcurrentDictionary<'a, 'b>()
  fun (x: 'a) ->
    cache.GetOrAdd(x, f)

let inline (++) x y =
  match x with
  | None -> y
  | _ -> x