module Day5

open System
open System.IO
open FSharpPlus
open System.Text.RegularExpressions

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

type ImmutableStack<'T> =
    | Empty
    | Stack of 'T * ImmutableStack<'T>

    member s.Push x = Stack(x, s)

    member s.PushMany (xs: 'T list) =
        match xs with
        | head :: tail -> (s.Push head).PushMany tail
        | [] -> s

    member s.Pop() =
      match s with
      | Empty -> failwith "Underflow"
      | Stack(t,remainder) -> (t, remainder)

    member s.PopMany (count: int): 'T list * ImmutableStack<'T> =
        match count with
        | 0 -> List.Empty, s
        | count ->
            let (popped, remainder) = s.Pop()
            let remainderPopped, finalRemainder = remainder.PopMany(count - 1)
            List.append (List.singleton popped) remainderPopped, finalRemainder

    member s.Peek() =
      match s with
      | Empty -> failwith "Contain no elements"
      | Stack(t, _) -> t

let tryGetArrayIndex (index: int) (xs: 'T array): 'T option =
    if index >= xs.Length then
        None
    else
        Some xs[index]

let parseCrate =
    function
    | "   " -> None
    | ParseRegex @"\[([A-Z])\]" [ crate ] -> Some crate
    | crate -> failwith $"Invalid crate {crate}"

let parseStacks (definitions: string array) =
    let parseLine (line: string) =
        line
        |> Seq.chunkBySize 4
        |> Seq.map ((Array.take 3) >> String.Concat >> parseCrate)
        |> Seq.toArray

    let crates = definitions |> Array.map parseLine
    let numberOfStacks = crates |> Seq.map Array.length |> Seq.max

    let getCrate (stackIndex: int) (crateIndex: int): string option =
        let crate = tryGetArrayIndex stackIndex crates[crateIndex]
        match crate with
        | None -> None
        | Some crate -> crate

    let createStack (stackIndex: int): ImmutableStack<string> =
        let addCrate (stack: ImmutableStack<string>) (crate: string option) =
            match crate with
            | Some crate -> stack.Push crate
            | None -> stack

        [ definitions.Length - 1 .. -1 .. 0 ]
        |> Seq.map (getCrate stackIndex)
        |> Seq.fold addCrate ImmutableStack.Empty

    Array.init numberOfStacks createStack

type Instruction =
    { NumberOfCrates: int
      Source: int
      Destination: int }

let parseInstruction (instruction: string): Instruction =
    match instruction with
    | ParseRegex @"move (\d+) from (\d+) to (\d+)" [ crates; source; destination ] ->
        { NumberOfCrates = Int32.Parse crates
          Source = Int32.Parse source - 1
          Destination = Int32.Parse destination - 1; }
    | _ -> failwith $"Invalid instruction {instruction}"

let rec executeInstructionPartOne (stacks: ImmutableStack<string> array) (instruction: Instruction): (ImmutableStack<string> array) =
    match instruction.NumberOfCrates with
    | 0 -> stacks
    | numberOfCrates ->
        printf $"Running instruction {instruction}\n\n"
        let (crate, newSource) = stacks[instruction.Source].Pop()
        let newDestination = stacks[instruction.Destination].Push(crate)
        let newStacks =
            stacks
            |> Common.replaceArrayElement instruction.Source newSource
            |> Common.replaceArrayElement instruction.Destination newDestination

        executeInstructionPartOne newStacks {instruction with  NumberOfCrates = numberOfCrates - 1 }

let executeInstructionPartTwo (stacks: ImmutableStack<string> array) (instruction: Instruction): (ImmutableStack<string> array) =
    printf $"Running instruction {instruction}\n\n"

    let (crates, newSource) = stacks[instruction.Source].PopMany instruction.NumberOfCrates
    let newDestination = stacks[instruction.Destination].PushMany (List.rev crates)
    stacks
        |> Common.replaceArrayElement instruction.Source newSource
        |> Common.replaceArrayElement instruction.Destination newDestination

let input = File.ReadAllText("./05/input.txt") |> String.split [ "\n\n" ] |> Seq.toArray
let crateDefinitions = input.[0] |> String.split ["\n"] |> Seq.toArray
let stacks = parseStacks (crateDefinitions[0..crateDefinitions.Length - 2])
let instructions = input.[1] |> String.split ["\n"] |> Seq.map parseInstruction

for stack in stacks do
    printf $"Initial stack: %A{stack}\n\n"

let execute executeInstruction () =
    let finalStacks = instructions |> Seq.fold executeInstruction stacks

    for stack in finalStacks do
        printf $"Final stack: %A{stack}\n\n"

    let result =
        finalStacks
        |> Seq.map (fun stack -> stack.Peek())
        |> String.concat ""

    result

let solve = {
    PartOne = execute executeInstructionPartOne;
    PartTwo = execute executeInstructionPartTwo
}