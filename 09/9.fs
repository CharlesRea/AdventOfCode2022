module Day9

open System.IO
open Common

let input = File.ReadLines("./09/input.txt")

let (|MoveCommand|_|) =
    function
    | "U" -> Some(0, 1)
    | "R" -> Some(1, 0)
    | "D" -> Some(0, -1)
    | "L" -> Some(-1, 0)
    | _ -> None

let parseCommand (command: string) =
    match command with
    | ParseRegex "(\w) (\d+)" [ MoveCommand vector; Int count ] -> Seq.init count (fun _ -> vector)
    | _ -> failwith $"Invalid command {command}"

let commands = input |> Seq.collect parseCommand

let add (a1, a2) (b1, b2) = a1 + b1, a2 + b2

module Part1 =
    let moveKnot (headX, headY) (tailX, tailY) : int * int =
        match (headX - tailX, headY - tailY) with
        | 2, _ -> (tailX + 1, headY)
        | -2, _ -> (tailX - 1, headY)
        | _, 2 -> (headX, tailY + 1)
        | _, -2 -> (headX, tailY - 1)
        | _ -> (tailX, tailY)

    let result () =
        let initialState =
            {| Head = (0, 0)
               Tail = (0, 0)
               Visited = Set.empty<int * int> |}

        let finalState =
            (initialState, commands)
            ||> Seq.fold (fun state command ->
                let head = add state.Head command
                let tail = moveKnot head state.Tail

                {| state with
                    Head = head
                    Tail = tail
                    Visited = state.Visited |> Set.add tail |})

        finalState.Visited |> Set.count

module Part2 =
    let moveKnot (headX, headY) (tailX, tailY) : int * int =
        match (headX - tailX, headY - tailY) with
        | 2, 2 -> (tailX + 1, tailY + 1)
        | 2, -2 -> (tailX + 1, tailY - 1)
        | -2, 2 -> (tailX - 1, tailY + 1)
        | -2, -2 -> (tailX - 1, tailY - 1)
        | 2, _ -> (tailX + 1, headY)
        | -2, _ -> (tailX - 1, headY)
        | _, 2 -> (headX, tailY + 1)
        | _, -2 -> (headX, tailY - 1)
        | _ -> (tailX, tailY)

    let result () =
        let initialState =
            {| Knots = Seq.replicate 10 (0, 0)
               Visited = Set.singleton (0, 0) |}

        let finalState =
            (initialState, commands)
            ||> Seq.fold (fun state command ->
                let head = (add (Seq.head state.Knots) command)
                let knots = (head, Seq.tail state.Knots) ||> Seq.scan moveKnot |> Seq.toList

                {| Knots = knots
                   Visited = state.Visited |> Set.add (Seq.last knots) |})

        finalState.Visited |> Set.count

let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
