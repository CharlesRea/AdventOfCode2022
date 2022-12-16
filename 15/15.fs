module Day15

open System.IO
open Common
open FSharpPlus.Data
open Microsoft.FSharp.Core.Operators.Checked

let input = File.ReadLines("./15/input.txt")

type Sensor =
    { Location: int64 * int64
      Beacon: int64 * int64
      Distance: int64 }

let manhattanDistance (x1: int64, y1: int64) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let parseLocation (input: string) =
    match input with
    | ParseRegex "Sensor at x=(.+), y=(.+): closest beacon is at x=(.+), y=(.+)" [ Int64 sensorX
                                                                                   Int64 sensorY
                                                                                   Int64 beaconX
                                                                                   Int64 beaconY ] ->
        { Location = sensorX, sensorY
          Beacon = beaconX, beaconY
          Distance = manhattanDistance (sensorX, sensorY) (beaconX, beaconY) }
    | _ -> failwith $"Invalid input {input}"

let sensors = input |> Seq.map parseLocation |> Seq.toArray

module Part1 =
    let rowToInvestigate = 2000000L

    let locationsWithoutBeacon
        ({ Location = (sensorX, sensorY)
           Beacon = (beaconX, beaconY)
           Distance = distance }: Sensor)
        : int64 seq =
        let coords = seq { sensorX - distance - 1L .. sensorX + distance + 1L }

        coords
        |> Seq.filter (fun x -> (manhattanDistance (x, rowToInvestigate) (sensorX, sensorY)) <= distance)
        |> Seq.filter (fun x -> (x, rowToInvestigate) <> (beaconX, beaconY))

    let result () =
        sensors |> Seq.collect locationsWithoutBeacon |> Seq.distinct |> Seq.length

module Part2 =
    let minCoord = 0L
    let maxCoord = 4_000_000L

    let inline clamp value = value |> max minCoord |> min maxCoord

    let beaconsInRow (y: int64) ({ Location = (sensorX, sensorY); Distance = distance }: Sensor): (int64 * int64) option =
        let radius = distance - (abs (sensorY - y))

        match radius with
        | x when x > 0 -> Some (clamp (sensorX - radius), clamp (sensorX + radius))
        | _ -> None

    let possibleBeacon (y: int64) =
        let rec findBeaconSlot (mergedRange: int64 * int64) (remainingRanges: list<int64*int64>): int64 option =
            match fst mergedRange with
            | 1L -> Some 0
            | 0L ->
                match remainingRanges with
                | range :: nextRanges ->
                    match snd mergedRange, fst range with
                    | upper, lower when upper >= lower -> findBeaconSlot (fst mergedRange, max (snd range) (snd mergedRange)) nextRanges
                    | upper, lower when upper = lower - 2L -> Some (upper + 1L)
                    | upper, lower -> failwith $"Gap of greater than two between {mergedRange} and {range}"
                | [] ->
                    match snd mergedRange with
                    | x when x = (maxCoord - 1L) -> Some maxCoord
                    | x when x < (maxCoord - 1L) -> failwith $"Unexpected upper bound on final range: {mergedRange}, y={y}"
                    | _ -> None
            | _ -> failwith $"Unexpected mergedRange lower bound: {mergedRange}"

        let ranges =
            sensors
            |> Seq.map (beaconsInRow y)
            |> Seq.filter Option.isSome
            |> Seq.map Option.get
            |> Seq.sortBy fst
            |> Seq.toList

        match ranges with
        | [] -> failwith $"No ranges in row {y}"
        | firstRange :: nextRanges -> findBeaconSlot firstRange nextRanges

    let result () =
        let (x, y) =
            seq { minCoord..maxCoord }
            |> Seq.map (fun y -> (possibleBeacon y, y))
            |> Seq.find (fun (x, y) -> x.IsSome)
            |> fun (x, y) -> (x.Value, y)

        x * 4000000L + y

let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
