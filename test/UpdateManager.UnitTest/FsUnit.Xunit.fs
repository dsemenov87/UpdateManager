module FsUnit.Xunit

#nowarn "25" "64"

open System
open Xunit
open Xunit.Sdk
open NHamcrest
open NHamcrest.Core
open Prelude.Either

module CustomMatchers =
  open System.Collections
  open System.Reflection

  let equal x = CustomMatcher<obj>(sprintf "Equals %A" x, fun a -> a = x)

  //TODO: Look into a better way of doing this.
  let equalWithin (t:obj) (x:obj) = CustomMatcher<obj>(sprintf "%s with a tolerance of %s" (x.ToString()) (t.ToString()),
                                                       fun a -> let actualParsed, actual = Double.TryParse(string a, System.Globalization.NumberStyles.Any, new System.Globalization.CultureInfo("en-US"))
                                                                let expectedParsed, expect = Double.TryParse(string x, System.Globalization.NumberStyles.Any, new System.Globalization.CultureInfo("en-US"))
                                                                let toleranceParsed, tol = Double.TryParse(string t, System.Globalization.NumberStyles.Any, new System.Globalization.CultureInfo("en-US"))
                                                                if actualParsed && expectedParsed && toleranceParsed then
                                                                    abs(actual - expect) <= tol
                                                                else false )

  let not' (x:obj) = match box x with
                     | null -> Is.Not<obj>(Is.Null())
                     | :? IMatcher<obj> as matcher -> Is.Not<obj>(matcher)
                     |  x -> Is.Not<obj>(CustomMatcher<obj>(sprintf "Equals %s" (x.ToString()), fun a -> a = x) :> IMatcher<obj>)

  let throw (t:Type) = CustomMatcher<obj>(string t,
                           fun f -> match f with
                                    | :? (unit -> unit) as testFunc ->
                                        try
                                          testFunc()
                                          false
                                        with
                                        | ex -> if ex.GetType() = t then true else false
                                    | _ -> false )

  let throwWithMessage (m:string) (t:Type) = CustomMatcher<obj>(sprintf "%s \"%s\"" (string t) m,
                                                  fun f -> match f with
                                                           | :? (unit -> unit) as testFunc ->
                                                               try
                                                                 testFunc()
                                                                 false
                                                               with
                                                               | ex -> if ex.GetType() = t && ex.Message = m then true else false
                                                           | _ -> false )

  let be = id

  let Null = Is.Null()

  let Empty = CustomMatcher<obj>("A non empty", fun o ->
                  match o with
                  | :? string as s    -> s.Trim() = ""
                  | :? list<_> as l   -> List.isEmpty l
                  | :? array<_> as a  -> Array.isEmpty a
                  | :? seq<_> as s    -> Seq.isEmpty s
                  | :? System.Collections.IEnumerable as e -> e |> Seq.cast |> Seq.isEmpty
                  | _ -> false)

  let EmptyString = CustomMatcher<obj>("A non empty string", fun s -> (string s).Trim() = "")

  let NullOrEmptyString = CustomMatcher<obj>("A not empty or not null string", fun s -> String.IsNullOrEmpty(unbox s))

  let True = CustomMatcher<obj>("True", fun b -> unbox b = true)

  let False = CustomMatcher<obj>("False", fun b -> unbox b = false)

  let NaN = CustomMatcher<obj>("NaN", fun x ->
      match x with
      | :? single as s -> System.Single.IsNaN(s)
      | :? double as d -> System.Double.IsNaN(d)
      | _ -> false)

  let unique = CustomMatcher<obj>("All items unique", fun (x:obj) ->
      let isAllItemsUnique x =
          let y = Seq.distinct x
          Seq.length x = Seq.length y
      match x with
      | :? list<_> as l   -> l |> isAllItemsUnique
      | :? array<_> as a  -> a |> isAllItemsUnique
      | :? seq<_> as s    -> s |> isAllItemsUnique
      | :? System.Collections.IEnumerable as e -> e |> Seq.cast |> isAllItemsUnique
      | _ -> false)

  let sameAs x = Is.SameAs<obj>(x)

  let greaterThan (x:obj) = CustomMatcher<obj>(string x,
                                       fun actual -> (unbox actual :> IComparable).CompareTo(unbox x) > 0)

  let greaterThanOrEqualTo (x:obj) = CustomMatcher<obj>(string x,
                                                fun actual -> (unbox actual :> IComparable).CompareTo(unbox x) >= 0)

  let lessThan (x:obj) = CustomMatcher<obj>(string x,
                                      fun actual -> (unbox actual :> IComparable).CompareTo(unbox x) < 0)

  let lessThanOrEqualTo (x:obj) = CustomMatcher<obj>(string x,
                                             fun actual -> (unbox actual :> IComparable).CompareTo(unbox x) <= 0)

  let endWith (x:string) = CustomMatcher<obj>(string x, fun s -> (string s).EndsWith x)

  let startWith (x:string) = CustomMatcher<obj>(string x, fun s -> (string s).StartsWith x)

  let haveSubstring (x:string) = CustomMatcher<obj>(string x, fun s -> (string s).Contains x)

  let ofExactType<'a> = CustomMatcher<obj>(typeof<'a>.ToString(), fun x -> (unbox x).GetType() = typeof<'a>)

  let instanceOfType<'a> = CustomMatcher<obj>(typeof<'a>.ToString(), fun x -> typeof<'a>.IsInstanceOfType(x))

  let contain x = CustomMatcher<obj>(sprintf "Contains %s" (x.ToString()),
                            fun c -> match c with
                                     | :? list<_> as l -> l |> List.exists(fun i -> i = x)
                                     | :? array<_> as a -> a |> Array.exists(fun i -> i = x)
                                     | :? seq<_> as s -> s |> Seq.exists(fun i -> i = x)
                                     | :? System.Collections.IEnumerable as e -> e |> Seq.cast |> Seq.exists(fun i -> i = x)
                                     | _ -> false)

  let private (?) (this : 'Source) (name : string) : 'Result =
      let bindingFlags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.GetProperty
      let property = this.GetType().GetProperty(name, bindingFlags)
      if (property = null) then
          raise (ArgumentException(sprintf "Property %s was not found" name, "name"))
      property.GetValue(this, null) :?> 'Result

  let haveLength n = CustomMatcher<obj>(sprintf "Have Length %d" n, fun x -> x?Length = n)

  let haveCount n = CustomMatcher<obj>(sprintf "Have Count %d" n, fun x -> x?Count = n)

  let containf f = CustomMatcher<obj>(sprintf "Contains %s" (f.ToString()),

                            fun c -> match c with
                                     | :? list<_> as l -> l |> List.exists f
                                     | :? array<_> as a -> a |> Array.exists f
                                     | :? seq<_> as s -> s |> Seq.exists f
                                     | :? System.Collections.IEnumerable as e -> e |> Seq.cast |> Seq.exists f
                                     | _ -> false)

  let matchList xs = CustomMatcher<obj>(sprintf "All elements from list %s" (xs.ToString()),
                            fun ys -> match ys with
                                      | :? list<_> as ys' -> List.sort xs = List.sort ys'
                                      | :? System.Collections.IEnumerable as e -> e |> Seq.cast |> Seq.isEmpty && xs |> Seq.isEmpty
                                      | :? _ -> false)

  let private makeOrderedMatcher description comparer =
      CustomMatcher<obj>(description,
          fun c -> match c with
                   | :? list<IComparable> as l -> l = List.sortWith comparer l
                   | :? array<IComparable> as a -> a = Array.sortWith comparer a
                   | :? seq<IComparable> as s ->
                           let a = s |> Seq.toArray
                           a = (a |> Array.sortWith comparer)
                   | :? System.Collections.IEnumerable as e ->
                           let a = e |> Seq.cast |> Seq.toArray
                           a = (a |> Array.sortWith comparer)
                   | _ -> false)

  let ascending = makeOrderedMatcher "Ascending" compare

  let descending = makeOrderedMatcher "Descending" (fun a b -> -(compare a b))

  type ChoiceDiscriminator(n : int) =
    member this.check(c : Choice<'a, 'b>): bool =
      match c with
        | Choice1Of2(_) -> n = 1
        | Choice2Of2(_) -> n = 2
    member this.check(c : Choice<'a, 'b, 'c>): bool =
      match c with
        | Choice1Of3(_) -> n = 1
        | Choice2Of3(_) -> n = 2
        | Choice3Of3(_) -> n = 3
    member this.check(c : Choice<'a, 'b, 'c, 'd>): bool =
      match c with
        | Choice1Of4(_) -> n = 1
        | Choice2Of4(_) -> n = 2
        | Choice3Of4(_) -> n = 3
        | Choice4Of4(_) -> n = 4
    member this.check(c : Choice<'a, 'b, 'c, 'd, 'e>): bool =
      match c with
        | Choice1Of5(_) -> n = 1
        | Choice2Of5(_) -> n = 2
        | Choice3Of5(_) -> n = 3
        | Choice4Of5(_) -> n = 4
        | Choice5Of5(_) -> n = 5
    member this.check(c : Choice<'a, 'b, 'c, 'd, 'e, 'f>): bool =
      match c with
        | Choice1Of6(_) -> n = 1
        | Choice2Of6(_) -> n = 2
        | Choice3Of6(_) -> n = 3
        | Choice4Of6(_) -> n = 4
        | Choice5Of6(_) -> n = 5
        | Choice6Of6(_) -> n = 6
    member this.check(c : Choice<'a, 'b, 'c, 'd, 'e, 'f, 'g>): bool =
      match c with
        | Choice1Of7(_) -> n = 1
        | Choice2Of7(_) -> n = 2
        | Choice3Of7(_) -> n = 3
        | Choice4Of7(_) -> n = 4
        | Choice5Of7(_) -> n = 5
        | Choice6Of7(_) -> n = 6
        | Choice7Of7(_) -> n = 7
    member this.check(c : obj): bool =
      let cType = c.GetType()
      let cArgs = cType.GetGenericArguments()
      let cArgCount = Seq.length cArgs
      try
        this.GetType().GetMethods()
        |> Seq.filter (fun m -> m.Name = "check"
                                && Seq.length (m.GetGenericArguments()) = cArgCount)
        |> Seq.exists (fun m -> m.MakeGenericMethod(cArgs).Invoke(this, [| c |]) :?> bool)
      with
        | _ -> false

  let choice n = CustomMatcher<obj>(sprintf "The choice %d" n,
                                      fun x -> (new ChoiceDiscriminator(n)).check(x))

type MatchException (expected, actual, userMessage) =
    inherit AssertActualExpectedException(expected, actual, userMessage)

type Xunit.Assert with
    static member That<'a> (actual, matcher:IMatcher<'a>) =
      if not (matcher.Matches(actual)) then
        let description = StringDescription()
        matcher.DescribeTo(description)
        let mismatchDescription = StringDescription()
        matcher.DescribeMismatch(actual, mismatchDescription)
        raise (MatchException(description.ToString(), mismatchDescription.ToString(), null))

let inline should (f : 'a -> ^b) x (y : obj) =
  let c = f x
  let y =
    match y with
    | :? (unit -> unit) as assertFunc -> box assertFunc
    | _ -> y
  if isNull (box c) then
    Assert.That(y, IsNullMatcher())
  else
    Assert.That(y, c)

let inline shouldSucceed (y : Choice< 'T, 'TError>) =
  match y with
  | Right y'   -> y'
  | Left _ -> raise (MatchException("Method should succeed", "Returned Error", null)) 

let inline shouldFail (y : Choice< 'T, 'TError>) =
  match y with
  | Left y'-> y'
  | Right _    -> raise (MatchException("Method should fail", "Returned Ok", null)) 


let inline shouldRaiseException (f:unit->unit) =
  let failed =
    try
      f()
      false
    with
    | _ -> true
  if not failed then
    raise (MatchException("Method should fail", "No exception raised", null))


let equal expected = CustomMatchers.equal expected
let equalWithin (tolerance:obj) (expected:obj) = CustomMatchers.equalWithin tolerance expected
let not' (expected:obj) = CustomMatchers.not' expected
let throw (t:Type) = CustomMatchers.throw t
let throwWithMessage (m:string) (t:Type) = CustomMatchers.throwWithMessage m t
let be = CustomMatchers.be
let inline shouldStrictEquals x y = Assert.StrictEqual(x, y)
let inline shouldBeSucceed x y = y |> shouldSucceed |> shouldStrictEquals x
let inline shouldBeFail x y = y |> shouldFail |> shouldStrictEquals x
/// Infix version of shouldBeSucceed
let inline (<==>) x y = shouldBeSucceed y x
/// Infix version of shouldBeFail
let inline (<?=>) x y = shouldBeFail y x
let Null = CustomMatchers.Null
let Empty = CustomMatchers.Empty
let EmptyString = CustomMatchers.EmptyString
let NullOrEmptyString = CustomMatchers.NullOrEmptyString
let True = CustomMatchers.True
let False = CustomMatchers.False
let NaN = CustomMatchers.NaN
let unique = CustomMatchers.unique
let sameAs expected = CustomMatchers.sameAs expected
let greaterThan (expected:obj) = CustomMatchers.greaterThan expected
let greaterThanOrEqualTo (expected:obj) = CustomMatchers.greaterThanOrEqualTo expected
let lessThan (expected:obj) = CustomMatchers.lessThan expected
let lessThanOrEqualTo (expected:obj) = CustomMatchers.lessThanOrEqualTo expected
let endWith (expected:string) = CustomMatchers.endWith expected
let startWith (expected:string) = CustomMatchers.startWith expected
let haveSubstring (expected:string) = CustomMatchers.haveSubstring expected
let ofExactType<'a> = CustomMatchers.ofExactType<'a>
let instanceOfType<'a> = CustomMatchers.instanceOfType<'a>
let contain expected = CustomMatchers.contain expected
let haveLength n = CustomMatchers.haveLength n
let haveCount n = CustomMatchers.haveCount n
let matchList = CustomMatchers.matchList
let choice = CustomMatchers.choice
let ascending = CustomMatchers.ascending
let descending = CustomMatchers.descending