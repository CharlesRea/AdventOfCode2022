#r @"nuget: FSharpPlus"

open System
open System.IO
open FSharpPlus

let priority (item: char) =
    match Char.IsUpper(item) with
    | true -> (int item) - (int 'A') + 27
    | false -> (int item) - int ('a') + 1

let scoreGroup (group: string seq) =
    let duplicates =
        group
        |> Seq.map (seq >> Set.ofSeq)
        |> Set.intersectMany
        |> Seq.toList

    match duplicates with
    | [duplicate] -> priority duplicate
    | duplicates -> failwith $"Invalid duplicates: {duplicates}"

let result =
    File.ReadLines("./input.txt")
    |> Seq.chunkBySize 3
    |> Seq.sumBy scoreGroup

printf $"%d{result}"