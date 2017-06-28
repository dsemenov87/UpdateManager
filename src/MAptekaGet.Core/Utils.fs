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