module Day2

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

let scoreOutcome =
    function
    | Win -> 6
    | Draw -> 3
    | Loss -> 0

module Part1 =
    let resolveRound =
        function
        | (Rock, Paper) -> Win
        | (Paper, Scissors) -> Win
        | (Scissors, Rock) -> Win
        | (x, y) when x = y -> Draw
        | _ -> Loss

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

    let result () =
        File.ReadLines("./02/input.txt")
        |> Seq.map parseInputLine
        |> Seq.sumBy scoreRound

module Part2 =
    let shapeToPlay =
        function
        | (shape, Draw) -> shape
        | (Rock, Loss) -> Scissors
        | (Paper, Loss) -> Rock
        | (Scissors, Loss) -> Paper
        | (Rock, Win) -> Paper
        | (Scissors, Win) -> Rock
        | (Paper, Win) -> Scissors

    let scoreRound (opponent, outcome) =
        (shapeToPlay (opponent, outcome) |> scoreShape) + scoreOutcome outcome

    let (|Opponent|_|) =
        function
        | "A" -> Some Rock
        | "B" -> Some Paper
        | "C" -> Some Scissors
        | _ -> None

    let (|Outcome|_|) =
        function
        | "X" -> Some Loss
        | "Y" -> Some Draw
        | "Z" -> Some Win
        | _ -> None

    let parseInputLine line =
        match line |> String.split [" "] |> Seq.toList with
        | [Opponent opponent; Outcome outcome] -> (opponent, outcome)
        | x -> failwith $"Invalid input {x}"

    let result () =
        File.ReadLines("./02/input.txt")
        |> Seq.map parseInputLine
        |> Seq.sumBy scoreRound

let solve = { PartOne = Part1.result; PartTwo = Part2.result }