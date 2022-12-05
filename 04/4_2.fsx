#r @"nuget: FSharpPlus"

open System
open System.IO
open FSharpPlus

let parseRange (range: string) =
    let split = range |> String.split ["-"] |> Seq.toList
    match split with
    | [lower; upper] -> (int lower, int upper)
    | _ -> failwith $"Invalid range {range}"


let parseLine (line: string) =
    let ranges = line |> String.split [","] |> Seq.toList
    match ranges with
    | [elfA; elfB] -> parseRange elfA, parseRange elfB
    | _ -> failwith $"Invalid line {line}"

let elfsOverlap (((x1, x2): int * int), ((y1, y2): int * int)) =
    x1 <= y2 && y1 <= x2

let result =
    File.ReadLines("./input.txt")
    |> Seq.map parseLine
    |> Seq.filter elfsOverlap
    |> Seq.length

printf $"%d{result}"