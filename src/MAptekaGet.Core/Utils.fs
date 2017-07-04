namespace MAptekaGet

[<AutoOpen>]
module Utils =
  let inline show x = x.ToString()
  let inline cons head tail = head::tail

  let inline words (text: string) = text.Split [|' '; '\n'|] |> Array.toList

  let inline (^) x = x

  module ResultExt =
    /// apply a wrapped function to a wrapped value
    let apply fP xP = Result.bind (fun f -> Result.bind (Ok << f) xP ) fP 

    /// lift a two parameter function to Result World
    let lift2 f xP yP = apply (apply (Ok f) xP) yP

    let applyR fR xR = Result.bind (fun f -> Result.bind f xR ) fR 
    
    /// Convert a list of Results into a Result of a list
    let rec sequence parserList =
      let consP = lift2 cons
      match parserList with
      | []         -> Ok []
      | head::tail -> consP head (sequence tail)

  module ResultOp =
    open ResultExt

    /// Infix version of Result.bind
    let inline (>>=) x f = Result.bind f x
    
    /// Infix version of Result.map
    let inline (<!>) x f = Result.map f x

    /// Infix version of Result.mapError
    let inline (<?>) x f = Result.mapError f x
    
    /// infix version of apply
    let ( <*> ) = apply
    /// pipeline version of apply
    let ( <|*> ) fP xP = apply xP fP
  
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



  /// A type-safe list that contains at least one element.
  type NonEmptyList<'t> =
    { Head: 't
      Tail: 't list
    } 

    interface System.Collections.Generic.IEnumerable<'t> with
      member x.GetEnumerator() =
        let {Head = x; Tail = xs} = x in (seq (x::xs)).GetEnumerator()
        
    interface System.Collections.IEnumerable with
      member x.GetEnumerator() =
        let {Head = x; Tail = xs} = x
        in (seq (x::xs)).GetEnumerator() :> System.Collections.IEnumerator
  
  /// Basic operations on NonEmptyList
  [<RequireQualifiedAccess>]
  module NonEmptyList =
    let head nel = let {Head = a; Tail = _} = nel in a
    let tail nel = let           {Tail = a} = nel in a
    let create x xs = {Head = x; Tail = xs}
    let singleton x = {Head = x; Tail = []}
    let toList {Head = x; Tail = xs} = x::xs
    let toSeq  {Head = x; Tail = xs} = seq { yield x; yield! xs; }
    let map f  {Head = x; Tail = xs} = {Head = f x; Tail = List.map f xs}
    let cons e {Head = x; Tail = xs} = {Head = e  ; Tail = x::xs}
    let rec tails s =
      let {Tail = xs} = s
      match xs with
      | []   -> {Head = s; Tail = []}
      | h::t -> cons s (tails {Head = h; Tail = t})
    
    let sortBy f {Head = x; Tail = xs} =
      match List.sortBy f (x::xs) with
      | [] -> failwith "can't be"
      | x::xs -> create x xs

    let reverse nel =
      nel
      |> Seq.rev
      |> (fun s ->
        match Seq.tryHead s with
        | Some h -> create h (s |> Seq.skip 1 |> Seq.toList)
        | None   -> nel
      )
  
  /// Lazy tree
  type Tree<'a> =
    | Node of 'a * Tree<'a> seq 
  
  [<RequireQualifiedAccess>]
  module Tree =
    let rec map f (Node (a, cs)) =
      Node (f a, Seq.map (map f) cs)
    
    let singleton x = Node (x, [])

    let fromNonEmptyList {Head = x; Tail = xs} =
      let rec fromList lst =
        match lst with
        | [] -> []
        | y::ys -> [Node (y, fromList ys)]
  
      Node (x, fromList xs)

  module Dist =
    /// Computes the restricted Damerau-Levenstein edit distance,
    /// also known as the "optimal string alignment" distance.
    ///  - read more at https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance
    ///  - Implementation taken from http://www.navision-blog.de/2008/11/01/damerau-levenshtein-distance-in-fsharp-part-ii/
    let private calcDamerauLevenshtein (a:string, b:string) =
      let m = b.Length + 1
      let mutable lastLine = Array.init m id
      let mutable lastLastLine = Array.zeroCreate m
      let mutable actLine = Array.zeroCreate m

      for i in 1 .. a.Length do
        actLine.[0] <- i
        for j in 1 .. b.Length do
          let cost = if a.[i-1] = b.[j-1] then 0 else 1
          let deletion = lastLine.[j] + 1
          let insertion = actLine.[j-1] + 1
          let substitution = lastLine.[j-1] + cost
          actLine.[j] <- 
            deletion 
            |> min insertion 
            |> min substitution

          if i > 1 && j > 1 then
            if a.[i-1] = b.[j-2] && a.[i-2] = b.[j-1] then
              let transposition = lastLastLine.[j-2] + cost  
              actLine.[j] <- min actLine.[j] transposition
      
        // swap lines
        let temp = lastLastLine
        lastLastLine <- lastLine
        lastLine <- actLine
        actLine <- temp
                
      lastLine.[b.Length]

    /// Calculates the edit distance between two strings.
    /// The edit distance is a metric that allows to measure the amount of difference between two strings 
    /// and shows how many edit operations (insert, delete, substitution) are needed to transform one string into the other.
    let restrictedDamerauLevenshteinDistance (a:string) (b:string) =
      if a.Length > b.Length then
        calcDamerauLevenshtein(a, b)
      else
        calcDamerauLevenshtein(b, a)
    
    /// Infix version of restrictedDamerauLevenshteinDistance
    let inline (<-->) x y =
      restrictedDamerauLevenshteinDistance (x.ToString()) (y.ToString())