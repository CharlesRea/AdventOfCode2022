module Day10

open System.IO
open Common
open FSharpPlus

let input = File.ReadLines("./10/input.txt")

type Instruction =
    | Addx of Value: int
    | Noop

let cycleLength =
    function
    | Addx _ -> 2
    | Noop -> 1

let updateRegister register =
    function
    | Addx value -> register + value
    | Noop -> register

let parseInstruction =
    function
    | ParseRegex "addx (.+)" [ Int value ] -> Addx value
    | "noop" -> Noop
    | instruction -> failwith $"Invalid instruction {instruction}"

type ProgramState = { Cycle: int; Register: int }

let instructions = input |> Seq.map parseInstruction

let states =
    instructions
    |> Seq.map (fun instruction ->
        let cycle = cycleLength instruction

        let registerIncrease =
            match instruction with
            | Addx value -> value
            | Noop -> 0

        seq {
            yield! Seq.replicate (cycle - 1) 0
            yield registerIncrease
        })
    |> Seq.collect id
    |> Seq.scan (fun register registerIncrease -> register + registerIncrease) 1
    |> Seq.mapi (fun cycle register ->
        { Cycle = cycle + 1
          Register = register })

module Part1 =
    let result () =
        states
        |> Seq.filter (fun state -> state.Cycle <= 220 && ((state.Cycle + 20) % 40 = 0))
        |> Seq.map (fun state -> state.Cycle * state.Register)
        |> Seq.sum

module Part2 =
    let result () =
        states
        |> Seq.map (fun state ->
            match (abs (((state.Cycle - 1) % 40) - state.Register)) with
            | x when x <= 1 -> "#"
            | _ -> ".")
        |> Seq.chunkBySize 40
        |> Seq.map (String.concat "")
        |> String.concat "\n"

let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
