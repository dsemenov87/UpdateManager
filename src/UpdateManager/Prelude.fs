[<AutoOpen>]
module Prelude

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

[<RequireQualifiedAccess>]
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

[<RequireQualifiedAccess>]
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

[<RequireQualifiedAccess>]
module AsyncSeq =
  let cons c cs =
    AsyncSeq.append (AsyncSeq.singleton c) cs 

[<AutoOpen>]
module Either =
  let (|Right|Left|) = function Choice1Of2 x -> Right x | Choice2Of2 y -> Left y

  let Right = Choice1Of2
  let Left = Choice2Of2

  type Either<'left, 'right> = Choice<'left, 'right>

[<RequireQualifiedAccess>]
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

module ChoiceInfixes =
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

[<RequireQualifiedAccess>]
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

