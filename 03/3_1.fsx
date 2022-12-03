#r @"nuget: FSharpPlus"

open System
open System.IO
open FSharpPlus

let priority (item: char) =
    match Char.IsUpper(item) with
    | true -> (int item) - (int 'A') + 27
    | false -> (int item) - int ('a') + 1

let parseCompartment (compartment: string) =
    compartment |> seq |> Set.ofSeq

let parseLine (line: string) =
    let compartmentSize = line.Length / 2
    (line[..compartmentSize - 1] |> parseCompartment, line[compartmentSize..] |> parseCompartment)

let scoreInputLine (line: string) =
    let duplicates =
        line
        |> parseLine
        |> fun (x, y) -> Set.intersect x y
        |> Set.toList

    match duplicates with
    | [duplicate] -> priority duplicate
    | duplicates -> failwith $"Invalid duplicates: {duplicates}"

let result =
    File.ReadLines("./input.txt")
    |> Seq.sumBy scoreInputLine

printf $"%d{result}"
