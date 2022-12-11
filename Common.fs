module Common

open System
open System.Numerics
open System.Text.RegularExpressions
open FSharpx.Collections

let tryParseWith (tryParseFunc: string -> bool * _) =
    tryParseFunc
    >> function
        | true, v -> Some v
        | false, _ -> None

let parseInt = tryParseWith Int32.TryParse
let parseInt64 = tryParseWith Int64.TryParse
let parseBigInt = tryParseWith BigInteger.TryParse
let parseDouble = tryParseWith Double.TryParse

let (|Int|_|) = parseInt
let (|Int64|_|) = parseInt64
let (|BigInteger|_|) = parseBigInt
let (|Double|_|) = parseDouble

let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None

let splitString (separator: string) (str: string) : string[] =
    str.Split([| separator |] |> Seq.toArray, StringSplitOptions.None)

let printSequence (value: 'a seq) =
    value |> Seq.toList |> List.iter (printf "%A\r\n")

let replaceArrayElement (index: int) (newValue: 'a) (array: 'a array) : 'a array =
    seq { for i in 0 .. array.Length - 1 -> if i = index then newValue else array.[i] }
    |> Seq.toArray

let indexed (xs: 'a seq) : ('a * int) seq = xs |> Seq.mapi (fun i x -> (x, i))

let toLookup (values: ('a * 'b) seq) : Map<'a, 'b list> =
    values
    |> Seq.groupBy fst
    |> Seq.map (fun (key, value) -> (key, Seq.map snd value |> Seq.toList))
    |> Map.ofSeq

let all (predicate: 'T -> bool) (values: 'T seq) : bool =
    not (Seq.exists (fun value -> not (predicate value)) values)

let allTriples (xs: 'x seq) (ys: 'y seq) (zs: 'z seq) : ('x * 'y * 'z) seq =
    zs
    |> Seq.allPairs ys
    |> Seq.allPairs xs
    |> Seq.map (fun (x, (y, z)) -> (x, y, z))

let allQuadruples (ws: 'w seq) (xs: 'x seq) (ys: 'y seq) (zs: 'z seq) : ('w * 'x * 'y * 'z) seq =
    zs
    |> Seq.allPairs ys
    |> Seq.allPairs xs
    |> Seq.allPairs ws
    |> Seq.map (fun (w, (x, (y, z))) -> (w, x, y, z))

let values (map: Map<'K, 'V>) : 'V seq = map |> Map.toSeq |> Seq.map snd

let joinCharsToString (chars: char seq) : string =
    chars |> Seq.map string |> String.concat ""

let reverseString x = x |> Seq.rev |> joinCharsToString
