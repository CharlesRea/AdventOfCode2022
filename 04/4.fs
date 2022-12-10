module Day4

open System.IO
open FSharpPlus

let parseRange (range: string) =
    let split = range |> String.split [ "-" ] |> Seq.toList

    match split with
    | [ lower; upper ] -> (int lower, int upper)
    | _ -> failwith $"Invalid range {range}"

let parseLine (line: string) =
    let ranges = line |> String.split [ "," ] |> Seq.toList

    match ranges with
    | [ elfA; elfB ] -> parseRange elfA, parseRange elfB
    | _ -> failwith $"Invalid line {line}"

let input = File.ReadLines("./04/input.txt") |> Seq.map parseLine

module Part1 =
    let oneElfContainsOther (((x1, y1): int * int), ((x2, y2): int * int)) =
        x1 >= x2 && y1 <= y2 || x2 >= x1 && y2 <= y1

    let result () =
        input |> Seq.filter oneElfContainsOther |> Seq.length

module Part2 =
    let elfsOverlap (((x1, x2): int * int), ((y1, y2): int * int)) = x1 <= y2 && y1 <= x2

    let result () =
        input |> Seq.filter elfsOverlap |> Seq.length

let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
