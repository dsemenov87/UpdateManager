module CommonUtils

open FParsec
open MAptekaGet.Utils.Parsing
open MAptekaGet.Domain.Parsing

let upd txt =
   match update <-- txt with Result.Ok x -> x | _ -> invalidArg "txt" "schould be valid update name."

let dep txt =
   match dependency <-- txt with Result.Ok x -> x | _ -> invalidArg "txt" "schould be valid dependency name."