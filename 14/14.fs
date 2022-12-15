module Day14

open System
open System.IO
open FSharpPlus

let input = File.ReadLines("./14/input.txt")

let rangeBetween x y =
    seq { x .. (if x > y then -1 else 1) .. y }

let parseInputLine (input: string) =
    input
    |> String.split [ " -> " ]
    |> Seq.map (fun coords ->
        coords
        |> String.split [ "," ]
        |> Seq.map Int32.Parse
        |> Seq.toList
        |> (fun coords -> coords[0], coords[1]))
    |> Seq.pairwise
    |> Seq.map (fun ((x1, y1), (x2, y2)) -> Seq.allPairs (rangeBetween x1 x2) (rangeBetween y1 y2))
    |> Seq.collect id

let rocks =
    input |> Seq.map parseInputLine |> Seq.collect id |> Seq.distinct |> Set.ofSeq

let sandEntryPoint = (500, 0)

module Part1 =
    let floor = rocks |> Seq.map snd |> Seq.max

    let rec dropSand (occupied: (int * int) Set) (x, y) : (int * int) option =
        // printf $"{(x, y)}; "
        match y with
        | y when y > floor -> None
        | _ ->
            match Set.contains (x, y + 1) occupied with
            | false -> dropSand occupied (x, y + 1)
            | true ->
                match Set.contains (x - 1, y + 1) occupied with
                | false -> dropSand occupied (x - 1, y + 1)
                | true ->
                    match Set.contains (x + 1, y + 1) occupied with
                    | false -> dropSand occupied (x + 1, y + 1)
                    | true when (x, y) = sandEntryPoint -> failwith "Room has filled up"
                    | true -> Some(x, y)

    let result () =
        let finalState =
            rocks
            |> Seq.unfold (fun occupied ->
                let nextSand = dropSand occupied sandEntryPoint

                match nextSand with
                | Some sand ->
                    let nextOccupied = Set.add sand occupied
                    Some(nextOccupied, nextOccupied)
                | None -> None)
            |> Seq.last

        Set.count finalState - Set.count rocks

module Part2 =
    let floor = (rocks |> Seq.map snd |> Seq.max) + 2

    let rec dropSand (occupied: (int * int) Set) (x, y) : (int * int) option =
        match y with
        | y when y = floor - 1 -> Some(x, y)
        | _ ->
            match Set.contains (x, y + 1) occupied with
            | false -> dropSand occupied (x, y + 1)
            | true ->
                match Set.contains (x - 1, y + 1) occupied with
                | false -> dropSand occupied (x - 1, y + 1)
                | true ->
                    match Set.contains (x + 1, y + 1) occupied with
                    | false -> dropSand occupied (x + 1, y + 1)
                    | true when (x, y) = sandEntryPoint -> None
                    | true -> Some(x, y)

    let result () =

        let finalState =
            rocks
            |> Seq.unfold (fun occupied ->
                let nextSand = dropSand occupied sandEntryPoint

                match nextSand with
                | Some sand ->
                    let nextOccupied = Set.add sand occupied
                    Some(nextOccupied, nextOccupied)
                | None -> None)
            |> Seq.last

        1 + Set.count finalState - Set.count rocks

let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
