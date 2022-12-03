#r @"nuget: FSharpPlus"

open System.IO
open FSharpPlus

type Shape = Rock | Paper | Scissors
type Outcome = Loss | Draw | Win
type Round = Shape * Shape

let scoreShape =
    function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let resolveRound =
    function
    | (Rock, Paper) -> Win
    | (Paper, Scissors) -> Win
    | (Scissors, Rock) -> Win
    | (x, y) when x = y -> Draw
    | _ -> Loss

let scoreOutcome =
    function
    | Win -> 6
    | Draw -> 3
    | Loss -> 0

let scoreRound (opponent, player) =
    scoreShape player + ((opponent, player) |> resolveRound |> scoreOutcome)

let (|Opponent|_|) =
    function
    | "A" -> Some Rock
    | "B" -> Some Paper
    | "C" -> Some Scissors
    | _ -> None

let (|Player|_|) =
    function
    | "X" -> Some Rock
    | "Y" -> Some Paper
    | "Z" -> Some Scissors
    | _ -> None

let parseInputLine line =
    match line |> String.split [" "] |> Seq.toList with
    | [Opponent opponent; Player player] -> (opponent, player)
    | x -> failwith $"Invalid input {x}"

let result =
    File.ReadLines("./2_input.txt")
    |> Seq.map parseInputLine
    |> Seq.sumBy scoreRound

printf $"%d{result}"