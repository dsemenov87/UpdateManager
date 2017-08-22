namespace MAptekaGet

#nowarn "0060" // Override implementations in augmentations are now deprecated.

[<AutoOpen>]
module Utils =
  // let show = string // use string instead
  let inline cons head tail = head::tail

  let inline words (text: string) = text.Split [|' '; '\n'|] |> Array.toList
  let inline lines (text: string) = text.Split [|'\r'; '\n'|] |> Array.toList
  let inline (^) x = x

  let env name =
    let eitherVar = System.Environment.GetEnvironmentVariable name
    if System.String.IsNullOrWhiteSpace eitherVar then
      Choice2Of2 ""
    else  
      Choice1Of2 eitherVar

  let validateType<'T> input =
    try Some(System.Convert.ChangeType(input, typeof<'T>) :?> 'T)
    with _ -> None

  module IO =
    open System
    
    let rec copyData (ins: IO.Stream) (outs: IO.Stream) = async {   
      let buf = Array.zeroCreate 1024
      let! bytes = ins.AsyncRead buf
      if bytes > 0 then
        do! outs.AsyncWrite(buf, 0, bytes)
        return! copyData ins outs
    }

    let compressFile (ins: IO.Stream) outName = async {
      use compressedFileStream = IO.File.Create outName
      use compressionStream =
        new IO.Compression.GZipStream(compressedFileStream, IO.Compression.CompressionMode.Compress)

      do! copyData ins compressionStream;
    }

    let compressFileToZip entryName (input: IO.Stream) outZipName = async {
      use zipStream = IO.File.Create outZipName
      use archive =
        new IO.Compression.ZipArchive (zipStream, IO.Compression.ZipArchiveMode.Create)
      use writeStream = archive.CreateEntry(entryName).Open()
      do! copyData input writeStream;
    }

    let calculateMd5 (ins: IO.Stream) =
      use md5 = Security.Cryptography.MD5.Create()
      let res = ins |> md5.ComputeHash |> Array.map (fun i -> i.ToString("X2")) |> Array.reduce (+)
      ins.Position <- 0L;
      res

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

    override this.ToString() =
      this |> Seq.map (sprintf "%O") |> String.concat ","

  type NEL<'a> = NonEmptyList<'a>

  /// Basic operations on NonEmptyList
  [<RequireQualifiedAccess>]
  module NEL =
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

  type Forest<'a> = Tree<'a> seq
  
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

    let rec toSeq (Node (a, cs)) =
      seq {
        yield a
        for c in cs do
          yield! toSeq c
      }

  
  module Option =
    /// Convert a seq of Options into a Option of a seq
    let rec sequence options =
      let consOp = Option.lift2 (fun head tail -> Seq.append (Seq.singleton head) tail)
      match Seq.tryHead options with
      | None      -> Some Seq.empty
      | Some head -> consOp head (options |> Seq.skip 1 |> sequence)
  
  open FSharp.Control
  
  type AsyncTree<'a> =
    | ANode of 'a * AsyncSeq<AsyncTree<'a>>

  [<RequireQualifiedAccess>]
  module AsyncTree =
    let rec map f (ANode (a, cs)) =
      ANode (f a, AsyncSeq.map (map f) cs)
    
    let singleton x = ANode (x, AsyncSeq.empty)

    let rec toAsyncSeq (ANode (a, cs)) =
      AsyncSeq.append (AsyncSeq.singleton a) (AsyncSeq.collect toAsyncSeq cs)

  module AsyncSeq =
    let cons c cs =
      AsyncSeq.append (AsyncSeq.singleton c) cs 
  
  [<AutoOpen>]
  module Either =
    let (|Right|Left|) = function Choice1Of2 x -> Right x | Choice2Of2 y -> Left y

    let Right = Choice1Of2
    let Left = Choice2Of2

    type Either<'left, 'right> = Choice<'left, 'right>
  
  module Choice =
    let rec sequence choices =
      let consC = Choice.lift2 cons
      match choices with
      | []         -> Right []
      | head::tail -> consC head (sequence tail)

    let rec sequenceNonEmpty {Head=h; Tail=t} =
      match Choice.lift2 cons h (sequence t) with
      | Left err -> Left err
      | Right [] ->
          match h with
          | Left err -> Left err
          | Right x  -> Right (NEL.singleton x)
      
      | Right (x::xs) -> Right (NEL.create x xs)

    let mapAsync f res = async {
      match res with
      | Left y -> return Left y
      | Right x -> let! r = f x in return Right r
    }

    let bindAsync f res = async {
      match res with
      | Left y -> return Left y
      | Right x -> let! r = f x in return match r with Right r1 -> Right r1 | Left y1 -> Left y1
    }

    let revAsync res = async {
      match res with
      | Left y -> return Left y
      | Right x -> let! x' = x in return Right x'
    }

    module Infixes =
      /// Infix version of Result.bind
      let inline (>>=) x f = Choice.bind f x
      
      /// Infix version of Result.map
      let inline (<!>) x f = Choice.map f x

      /// Infix version of Result.mapError
      let inline (<?>) x f = Choice.mapSnd f x
      
      /// infix version of apply
      let ( <*> ) = Choice.apply
      /// pipeline version of apply
      let ( <|*> ) fP xP = Choice.apply xP fP

      let (>=>) f g x = (f x) >>= g 
  
  module Map =
    let merge m1 m2 = Map.fold (fun acc k v -> Map.add k v acc) m1 m2

  module Parsing =
    open FParsec
    
    let inline rf<'a> : Parser<char option, unit> =
      opt (pchar '\r') .>> pchar '\n' <?> "end of line"

    /// applies the parser p, ignores the result, and returns x.
    let inline (>>%) p x = p |>> (fun _ -> x)

    let inline toChoice res =
      match res with
      | Success (res,_,_) -> Right res
      | Failure _         -> Left (sprintf "%A" res)
    
    let inline parse parser txt = 
      run parser txt |> toChoice

    /// Infix version of parse
    let inline (<--) parser txt = parse parser txt
  
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
      restrictedDamerauLevenshteinDistance (string x) (string y)

  module PrettyPrint = 
    // Copyright (C) by Vesa Karvonen
    open System.IO

    type Doc =
      | Empty
      | Lazy of Lazy<Doc>
      | Line of Doc
      | Join of lhs: Doc * rhs: Doc
      | Nest of string * Doc
      | Text of string
      | Choice of wide: Doc * narrow: Doc
      | With of (int -> string -> Doc)
      | User of obj

    [<AutoOpen>]
    module Consts =
      let space = Text " "
      let line = Line space
      let linebreak = Line Empty
      let softline = Choice (space, line)
      let softbreak = Choice (Empty, linebreak)

      let inline join3 l d r = Join (l, Join (d, r))

    type Doc with
      static member (<^>)  (l, r) = Join (l, r)
      static member (<+>)  (l, r) = join3 l space     r
      static member (<.>)  (l, r) = join3 l line      r
      static member (</>)  (l, r) = join3 l softline  r
      static member (<..>) (l, r) = join3 l linebreak r
      static member (<//>) (l, r) = join3 l softbreak r

    [<AutoOpen>]
    module Util =
      let inline (^) x = x
      let inline constant x _ = x
      let inline flip f x y = f y x
      let inline force (s: Lazy<_>) = s.Force ()

    [<AutoOpen>]
    module PPrint =
      let delay th = Lazy ^ lazy th ()

      let empty = Empty
      let space = space
      let line = line
      let linebreak = linebreak
      let softline = softline
      let softbreak = softbreak

      let inline column' withCol = With ^ fun col _ -> withCol col
      let inline nesting' withNest = With ^ fun _ prefix -> withNest prefix.Length

      let column withCol = column' withCol
      let nesting withNest = nesting' withNest

      let txt s = Text s
      let user (x: obj) = User x
      let fmt f = Printf.ksprintf Text f
      let chr (c: char) = Text ^ string c

      let (lparen,   rparen)   as lrparen   = (Text "(", Text ")")
      let (langle,   rangle)   as lrangle   = (Text "<", Text ">")
      let (lbrace,   rbrace)   as lrbrace   = (Text "{", Text "}")
      let (lbracket, rbracket) as lrbracket = (Text "[", Text "]")

      let squote = Text "'"
      let lrsquote = (squote, squote)
      let dquote = Text "\""
      let lrdquote = (dquote, dquote)

      let semi      = Text ";"
      let colon     = Text ":"
      let comma     = Text ","
      let dot       = Text "."
      let backslash = Text "\\"
      let equals    = Text "="

      let punctuate sep (ds: seq<Doc>) = seq {
        use ds = ds.GetEnumerator ()
        if ds.MoveNext () then
          let mutable d = ds.Current
          while ds.MoveNext () do
            yield d <^> sep
            d <- ds.Current
          yield d
      }

      let inline spaces n = String.replicate n " "

      let nestBy s d = Nest (s, d)
      let nest n = nestBy ^ spaces n

      let align d = With ^ fun c p -> d |> nest ^ c-p.Length
      let hang i d = align ^ nest i d
      let indent i d = txt ^ spaces i <^> d |> hang i

      let inline width' d f = column' ^ fun l -> d <^> column' ^ fun r -> f ^ r-l
      let width d f = width' d f

      let inline mkFill p t f d =
        width' d ^ fun w -> if p f w then t f else txt ^ spaces ^ f-w

      let fillBreak n d = mkFill (<) (flip nest linebreak) n d
      let fill n d = mkFill (<=) (constant empty) n d

      let rec flatten doc = delay ^ fun () ->
        match doc with
         | Lazy doc -> flatten ^ force doc
         | Join (lhs, rhs) -> Join (flatten lhs, flatten rhs)
         | Nest (txt, doc) -> Nest (txt, flatten doc)
         | Empty | Text _ | User _ -> doc
         | Line wide -> wide
         | Choice (wide, _) -> wide
         | With f -> With <| fun c p -> f c p |> flatten

      let choice wide narrow = Choice (flatten wide, narrow)
      let group doc = choice doc doc

      let gnest n = group >> nest n

      let inline joinWith' bop xs =
        match Seq.fold (fun ys x -> x::ys) [] xs with
         | [] -> empty
         | x::xs -> List.fold (flip bop) x xs
      let joinWith bop xs = joinWith' bop xs

      let joinSep sep xs = xs |> joinWith' ^ fun l r -> join3 l sep r

      let hsep    xs = joinWith' (<+>)  xs
      let hcat    xs = joinWith' (<^>)  xs
      let vsep    xs = joinWith' (<.>)  xs
      let vcat    xs = joinWith' (<..>) xs
      let fillSep xs = joinWith' (</>)  xs
      let fillCat xs = joinWith' (<//>) xs

      let sep xs = group ^ vsep xs
      let cat xs = group ^ vcat xs

      let inline enclose' (l, r) d = join3 l d r
      let enclose (l, r) d = enclose' (l, r) d

      let squotes  d = enclose' lrsquote  d
      let dquotes  d = enclose' lrdquote  d
      let parens   d = enclose' lrparen   d
      let angles   d = enclose' lrangle   d
      let braces   d = enclose' lrbrace   d
      let brackets d = enclose' lrbracket d

      type t =
       | Nil
       | Print of text: string * Lazy<t>
       | Linefeed of prefix: string * Lazy<t>
       | Obj of obj: obj * Lazy<t>

      type [<AbstractClass>] Actions () =
        abstract Line: unit -> unit
        abstract Write: string -> unit
        abstract User: obj -> unit
        default t.Line () = t.Write "\n"
        default t.User _ = ()

      let rec output (actions: Actions) doc =
        match force doc with
         | Nil -> ()
         | Print (str, doc) ->
           actions.Write str
           output actions doc
         | Obj (obj, doc) ->
           actions.User obj
           output actions doc
         | Linefeed (prefix, doc) ->
           actions.Line ()
           actions.Write prefix
           output actions doc

      let rec fits maxCols usedCols doc =
        usedCols <= maxCols &&
        match force doc with
         | Nil | Linefeed _ -> true
         | Obj (_, doc) ->
           fits maxCols usedCols doc
         | Print (str, doc) ->
           fits maxCols (usedCols + str.Length) doc

      type Docs =
        | Done
        | Docs of string * Doc * Docs

      let rec layout maxColsOr0 usedCols = function
        | Done -> Nil
        | Docs (prefix, doc, rest) ->
          match doc with
           | Lazy doc ->
             layout maxColsOr0 usedCols ^ Docs (prefix, force doc, rest)
           | Empty ->
             layout maxColsOr0 usedCols rest
           | Join (lhs, rhs) ->
             layout maxColsOr0 usedCols ^
             Docs (prefix, lhs, Docs (prefix, rhs, rest))
           | Nest (txt, doc) ->
             layout maxColsOr0 usedCols ^ Docs (prefix + txt, doc, rest)
           | Text str ->
             Print (str, lazy layout maxColsOr0 (usedCols + str.Length) rest)
           | User obj ->
             Obj (obj, lazy layout maxColsOr0 usedCols rest)
           | Line _ ->
             Linefeed (prefix, lazy layout maxColsOr0 prefix.Length rest)
           | Choice (wide, narrow) ->
             let wide = layout maxColsOr0 usedCols ^ Docs (prefix, wide, rest)
             if maxColsOr0 = 0 || fits maxColsOr0 usedCols ^ lazy wide
             then wide
             else layout maxColsOr0 usedCols ^ Docs (prefix, narrow, rest)
           | With f ->
             layout maxColsOr0 usedCols ^ Docs (prefix, f usedCols prefix, rest)

      let outputWithActions actions maxCols doc =
        let maxColsOr0 =
          match maxCols with
           | None -> 0
           | Some n ->
             if n <= 0 then failwithf "maxCols: %d" n else n
        output actions ^ lazy layout maxColsOr0 0 (Docs ("", doc, Done))

      let inline outputWithFun write maxCols doc =
        outputWithActions
          {new Actions () with
            member t.Write s = write s}
          maxCols
          doc

      let outputToWriter (tw: TextWriter) maxCols doc =
        outputWithFun tw.Write maxCols doc

      let render maxCols doc =
        use tw = new StringWriter ()
        outputToWriter tw maxCols doc
        tw.ToString ()

      let println maxCols doc =
        outputWithFun System.Console.Write maxCols doc
        System.Console.Write "\n"

    module Json =
      open Chiron
      
      let private prettySeq (l, r) toDoc xs =
        if Seq.isEmpty xs
        then l <^> r
        else let ds = xs |> Seq.map toDoc |> punctuate comma |> vsep
             l <..> ds |> nest 2 <..> r |> group

      let rec pretty (json: Json) =
        match json with
        | Object jobj ->
         jobj
         |> JsonObject.toMap
         |> prettySeq lrbrace ^ fun kv ->
              pretty ^ Json.String kv.Key <^> colon <+> pretty kv.Value

        | Array jsons ->
         prettySeq lrbracket pretty jsons

        | _ -> json |> string |> txt

  [<RequireQualifiedAccess>]
  module Reporting =
    open PrettyPrint

    let inline showNode x y = sprintf "%O => %O" x y

    type Message =
      { Summary : string
        Details : Doc list
      }

      static member New (s: string) = fun d ->
        { Summary = s; Details = d }

      static member Zero
        with get () =
          Message.New "" []

    type IToReportMessage =
      abstract member ToReportMessage: unit -> Message

    /// Neat 2-dimensional drawing of a tree.
    let rec drawTree (Node (s, ts): Tree<string>) : Doc =
      let shift first other br =
        chr '|' <.> ((txt first <+> drawTree br) |> nestBy other)
      let rec drawBranches = function
        | []      -> Doc.Empty
        | [br]    -> shift "`-" "   " br
        | br::brs -> shift "+-" "|  " br <.> (drawBranches brs) 
      in
        txt s <.> (ts |> Seq.toList |> drawBranches)
    let private verticalAppend (a: Doc) (b: Doc) : Doc =
      a <^> line <^> line <^> b

    let private stack (allDocs: Doc list) : Doc =
      match allDocs with
      | [] ->
          failwith "Do not use `stack` on empty lists."
      
      | doc::docs ->
          List.fold verticalAppend doc docs
     
    let private start mode : Doc =
      softbreak <^> (mode |> string |> txt) <^> txt ":"

    let internal pmsgToDoc (pmsg: Message) : Doc =
      let {Summary=summary;Details=details} = pmsg
      let allDocs =
        if System.String.IsNullOrWhiteSpace summary then
          details
        else
          let summaryDoc =
            fillSep (summary |> words |> List.map txt)
          in
            summaryDoc :: (details |> List.map (indent 4))
      in  
        stack allDocs
        <^> line
        <^> line

    let inline choiceToMsg res =
      let msg =
        match res with
        | Choice1Of2 res -> string res
        | Choice2Of2 err -> string err
      in
        Message.New "" [txt msg]

    type Message with
      override x.ToString() =
        x|> pmsgToDoc |> render ^ Some 78

module Sql =
  open System
  open System.Data
  open System.Data.Common
  open Npgsql
  module Either = Result

  /// Eq - '=', Gt - '>', Ge - '>=', Lt - '<', Le - '<=' 
  type Op =
    | Eq | Gt | Ge | Lt | Le     
    
    override x.ToString() = match x with
                            | Eq -> "="
                            | Gt -> ">"
                            | Ge -> ">="
                            | Lt -> "<"
                            | Le -> "<="
  
  type OrdDir =
    | Asc | Desc

    override x.ToString() = match x with Asc -> "ASC" | Desc -> "DESC"
  
  // type SqlValue =
  //   | Integer of int32
  //   | Text of string

  type SqlError =
    | ColumnNotFound  of string
    | CastError       of string * string * Type
    | Unknown         of string

    override this.ToString() =
      match this with
      | ColumnNotFound c ->
          sprintf "Database column '%s' not found." c
      | CastError (c, t1, t2) ->
          sprintf "Cast error while reading column '%s': expected '%s' but instead '%O'." c t1 t2
      | Unknown err ->
          sprintf "Data access error: '%s'." err

  type RowReader<'v> = DbDataReader -> Either<'v * DbDataReader, SqlError>

  let returnR x r = Right (x, r)

  // type JoinType = InnerJoin | LeftJoin  

  // type Join = string * JoinType * Where option    // table name, join, optional "on" clause 
  
  type Select<'v, 't when 't: (static member Name: unit -> string)> = RowReader<'v> * SelectDetails<'t>
  
  and SelectDetails<'t> =
    { Tbl   : 't
      Proj  : string list
      Cond  : Map<string, Op * string>
      OrdBy : Map<string, OrdDir>
      Offst : int32 option
      Limit : int32 option
    }

  let showTxt = sprintf "'%s'"

  let showUuid (uuid: Guid) = uuid.ToString("N") |> sprintf "'%s'"

  let inline showInt x = sprintf "%d" x

  let showBln = sprintf "%b"

  [<RequireQualifiedAccess>]
  module Insert =
    let into tblName input =
      let names = List.map fst input
      let ins =
        input
        |> List.map fst
        |> String.concat ", "
        |> sprintf "INSERT INTO %s(%s)" tblName

      let vals =
        input
        |> List.map snd
        |> String.concat ", "
        |> sprintf "VALUES (%s)"
      in
        String.concat "\n" [ins; vals]
    
    let inline toParam (show: 'v -> string) (v: 'v) ((_,det): Select<'v, _>) =
      (List.head det.Proj, show v)

    let inline pTxt x = toParam showTxt x
    let inline pInt x = toParam showInt x
    let inline pBln x = toParam showBln x

    let inline pUuid x = toParam showUuid x
  
  let itRowAsync (dr: DbDataReader) (rr: RowReader<'v>) =
    async {
      try
        let! read = dr.ReadAsync() |> Async.AwaitTask
        if read then
          match rr dr with
          | Right (row,_) -> return Some ^ Right row
          | Left err      -> return Some ^ Left err
        else
          return None
      
      with ex ->
        return Some ^ Left ^ Unknown (string ex)
    }
  
  let private asTyp (n: string) (o: obj) =
    match o with
    | null  -> Left ^ ColumnNotFound n
    | _     -> Right o

  let private asNullable (n: string) (o: obj) =
    match o with
    | :? DBNull -> Left None
    | _         -> Right o

  let asInt (n: string) (r: DbDataReader) =
    r.[n] |> asTyp n |> Choice.bind (function
      | :? Int32 as num ->
          Right (num, r)
      | x ->
          Left ^ CastError ("int", n, x.GetType()))

  let inline private col<'a,'t> (chooseFn: string -> 'a) (name: string) = 
    ( chooseFn name,
      {Tbl=Unchecked.defaultof<'t>; Proj = [name]; Cond=Map.empty; OrdBy = Map.empty; Offst=None; Limit=None}
    )
    
  
  [<RequireQualifiedAccess>]
  module Select =

    let withOffset offst (r, det) = (r, {det with Offst = Some offst})

    let withLimit limit (r, det) = (r, {det with Limit = Some limit})

    let private showTxtCond ((op: Op), (x: string)) = (op, showTxt x)

    let inline private showIntCond ((op: Op), x) = (op, showInt x)

    let private showBlnCond ((op: Op), (x: bool)) = (op, showBln x)
    
    let inline toRawSql ((r, det): Select<'v, 't>) =
      let inline tblName (_:^t) = (^t: (static member Name: unit -> string) ())
      
      let select =
        if List.isEmpty det.Proj then
          "SELECT *"
        else
          "SELECT " + String.concat ", " det.Proj
      let from' = "FROM " + (tblName Unchecked.defaultof<'t>)
      let where = det.Cond |> Map.fold (fun acc k (op, v) -> sprintf "%s AND %s %O %s" acc k op v) "WHERE 1=1"
      let orderBy =
        if Map.isEmpty det.OrdBy then
          ""
        else
          "ORDER BY " + String.concat ", " (det.OrdBy |> Map.toList |> List.map (fun (n, od) ->  sprintf "%s %O" n od))
      let offset =
        match det.Offst with Some offst -> sprintf "OFFSET %d" offst | _ -> ""
      let limit =
        let num =
          match det.Limit with Some lmt -> lmt | _ -> 512
        in
          sprintf "LIMIT %d" num
      let cmdText =
        String.concat "\n" [select; from'; where; orderBy; offset; limit]
      in
        (r, cmdText)

    let toSeq (conn: NpgsqlConnection) (sqlReader, cmdText) = async {
      use cmd = conn.CreateCommand()
      cmd.CommandText <- cmdText

      if conn.State <> ConnectionState.Open then
        do! conn.OpenAsync () |> Async.AwaitTask

      use! rd = cmd.ExecuteReaderAsync () |> Async.AwaitTask
   
      let rec loop (rarr: ResizeArray<_>) = async {
        let! row = itRowAsync rd sqlReader
        match row with
        | Some data ->
            rarr.Add data;
            return! loop rarr
        | None ->
            return rarr
      }

      let! res = ResizeArray() |> loop
      return res :> System.Collections.Generic.IEnumerable<_>
    }

    let inline where show ((op, cond): Op * 'a) ((rr, det): Select<'a,_>) =
      (rr, {det with Cond = Map.add (List.head det.Proj) (op, show cond) det.Cond })

    let inline orderBy (ord: OrdDir) ((rr, (det)): Select<_,_>) =
      (rr, {det with OrdBy = Map.add (List.head det.Proj) ord det.OrdBy })

  let exec (conn: NpgsqlConnection) cmdText = async {
    use cmd = conn.CreateCommand()
    cmd.CommandText <- cmdText

    try
      if conn.State <> ConnectionState.Open then
        do! conn.OpenAsync () |> Async.AwaitTask

      let! cnt = cmd.ExecuteNonQueryAsync () |> Async.AwaitTask
      
      return Right cnt
    with ex ->
      return Left (Unknown ^ string ex)
  }

  let asBool (n: string) (r: DbDataReader) =
    r.[n] |> asTyp n |> Choice.bind (function
      | :? bool as b ->
          Right (b, r)
      | x ->
          Left ^ CastError ("bool", n, x.GetType()))

  let asDateTime (n: string) (r: DbDataReader) =
    r.[n] |> asTyp n |> Choice.bind (function
      | :? DateTime as dt ->
          Right (dt, r)
      | x ->
          Left ^ CastError ("datetime", n, x.GetType()))

  let asUuid (n: string) (r: DbDataReader) =
    r.[n] |> asTyp n |> Choice.bind (function
      | :? Guid as uuid ->
          Right (uuid, r)
      | x ->
          Left ^ CastError ("uuid", n, x.GetType()))

  let asTxt (n: string) (r: DbDataReader) =
    r.[n] |> asTyp n |> Choice.bind (fun x -> Right (string x, r))


  let inline intgr<'t> n  = col<_,'t> asInt n
  
  let inline txt<'t> n = col<_,'t> asTxt n

  let inline booln<'t> n = col<_,'t> asBool n

  let inline dtime<'t> n = col<_,'t> asDateTime n

  let inline uuid<'t> n = col<_,'t> asUuid n
  
  let private runR (reader,_) = reader

  let bindR (f: 'a -> RowReader<'b>) (a: RowReader<'a>) : RowReader<'b> =
    let readerFn r =
      match a r with
      | Left problem -> Left problem
      | Right (v, r) -> let b = f v in b r
    in
      readerFn

  /// apply a wrapped function to a wrapped value
  // let applyR fR xR = fR >>= (fun f -> xR >>= (returnR << f))

  // /// infix version of apply
  // let ( <*> ) = applyR

  // let lift2 f xS yS = returnR f <*> xS <*> yS 

  let mergeDet det1 det2 =
    let proj3 = det2.Proj @ det1.Proj |> List.distinct
    let ordBy3 = Map.fold (fun acc k v -> Map.add k v acc) det1.OrdBy det2.OrdBy
    let cond3 = Map.fold (fun acc k v -> Map.add k v acc) det1.Cond det2.Cond
    let offst3 =
      match det1.Offst, det2.Offst with
      | Some offst1, Some offst2  -> Some (max offst1 offst2)
      | Some offst1, None         -> Some offst1
      | None, Some offst2         -> Some offst2
      | _                         -> None
    let lmt3 =
      match det1.Limit, det2.Limit with
      | Some lmt1, Some lmt2  -> Some (max lmt1 lmt2)
      | Some lmt1, None       -> Some lmt1
      | None, Some lmt2       -> Some lmt2
      | _                     -> None
    in {Tbl=det1.Tbl; Proj=proj3; Cond=cond3; OrdBy=ordBy3; Offst=offst3; Limit=lmt3}

  let inline andThen ((rr1, stm1): Select<'a, 't>) ((rr2, stm2): Select<'b,'t>) : Select<'a * 'b,'t> =         
    let readerFn dr =
      match rr1 dr with
      | Left err -> Left err
      | Right (a, dr1) -> match rr2 dr1 with
                          | Left err        -> Left err
                          | Right (b, dr2)  -> Right ((a, b), dr2)
    in
      (readerFn, mergeDet stm1 stm2)

  let inline andThenIgnoring ((rr1, det1): Select<'a, 't>) ((rr2, det2): Select<'b,'t>) : Select<'a,'t> =     
    let ordBy3 = Map.fold (fun acc k v -> Map.add k v acc) det1.OrdBy det2.OrdBy
    let cond3 = Map.fold (fun acc k v -> Map.add k v acc) det1.Cond det2.Cond
    in
      (rr1, {det1 with OrdBy = ordBy3; Cond=cond3})

  module Operators =
  
    /// Infix version of bindP
    let ( >>= ) p f = bindR f p
    
    /// Lift a value to a Sql
    // let returnS x = (returnR x, emptyStm)

    let mapR f = bindR (f >> returnR)

    /// infix version of mapS
    let ( <!> ) = mapR

    let ( |>> ) (r, y) f = (mapR f r, y)
    
    /// Infix version of andThen
    let inline ( .>>. ) x = andThen x

    /// Infix version of andThenIgnoring
    let inline ( .>> ) x = andThenIgnoring x