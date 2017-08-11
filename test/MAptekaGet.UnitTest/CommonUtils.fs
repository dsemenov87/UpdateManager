module CommonUtils

open FParsec
open MAptekaGet.Utils.Parsing
open MAptekaGet.Domain.Parsing
open MAptekaGet.Utils.Either

let upd txt =
   match update <-- txt with Right x -> x | _ -> invalidArg "txt" "schould be valid update name."

let dep txt =
   match dependency <-- txt with Right x -> x | _ -> invalidArg "txt" "schould be valid dependency name."