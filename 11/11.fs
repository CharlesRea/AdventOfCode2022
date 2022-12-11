module Day11

open System.IO
open System.Numerics
open FSharpPlus
open Common
open System
open Microsoft.FSharp.Core.Operators.Checked

let input = File.ReadAllText("./11/input.txt")

type Test =
    { DivisibleBy: int64
      IfTrue: int
      IfFalse: int }

    member this.Evaluate(item: int64) : int =
        match (item % this.DivisibleBy) with
        | 0L -> this.IfTrue
        | _ -> this.IfFalse

type Monkey =
    { Items: int64 list
      ItemsInspected: int64
      Operation: int64 -> int64
      Test: Test }

    member this.AddItem(item: int64) : Monkey =
        { this with Items = List.append this.Items [item] }

    member this.MarkItemsInspected() : Monkey = { this with ItemsInspected = this.ItemsInspected + int64 this.Items.Length; Items = List.empty }

let parseStartingItems =
    function
    | ParseRegex "Starting items: (.*)" [ itemsInput ] ->
        itemsInput
        |> String.split [ "," ]
        |> Seq.map (String.trimWhiteSpaces >> Int64.Parse)
        |> Seq.toList
    | items -> failwith $"Invalid starting items: {items}"

let parseOperation (input: string) : int64 -> int64 =
    let (|Symbol|_|) =
        function
        | "*" -> Some (*)
        | "+" -> Some (+)
        | "-" -> Some (-)
        | _ -> None

    let (|Value|_|) =
        function
        | "old" -> Some id
        | Int64 x -> Some(fun _ -> x)
        | _ -> None

    match input with
    | ParseRegex "Operation: new = (.+) (.) (.+)" [ Value lhs; Symbol symbol; Value rhs ] ->
        fun old -> symbol (lhs old) (rhs old)
    | _ -> failwith $"Invalid operation: {input}"

let parseTest (input: string[]) : Test =
    let (|DivisibleBy|_|) =
        function
        | ParseRegex "Test: divisible by (\d+)" [ Int64 value ] -> Some value
        | _ -> None

    let (|ThrowTo|_|) =
        function
        | ParseRegex "If (.+): throw to monkey (.+)" [ condition; Int monkey ] -> Some(condition, monkey)
        | _ -> None

    match input with
    | [| DivisibleBy divisibleBy; ThrowTo("true", ifTrue); ThrowTo("false", ifFalse) |] ->
        { DivisibleBy = divisibleBy
          IfTrue = ifTrue
          IfFalse = ifFalse }
    | _ -> failwith $"Invalid test: %A{input}"

let parseMonkey (input: string array) =
    { Items = parseStartingItems input[1]
      ItemsInspected = 0
      Operation = parseOperation input[2]
      Test = parseTest input[3..5] }

let monkeys =
    input
    |> String.split [ "\n\n" ]
    |> Seq.map (
        String.split [ "\n" ]
        >> (Seq.map String.trimWhiteSpaces)
        >> Seq.toArray
        >> parseMonkey
    )
    |> Seq.toArray

let printMonkeys monkeys =
    for monkey in monkeys do
        printf $"Monkey inspected {monkey.ItemsInspected}. Holding:  %A{monkey.Items}\n"
    printf "\n\n"

type MonkeyResult = { Item: int64; ThrowTo: int }

let processMonkey (worryAfterInspection: Monkey -> int64 -> int64) (monkey: Monkey) : MonkeyResult seq =
    let processItem (item: int64) =
        let finalItem = item |> monkey.Operation |> (worryAfterInspection monkey)

        { Item = finalItem
          ThrowTo = monkey.Test.Evaluate finalItem }

    monkey.Items |> Seq.map processItem

let applyResult (monkeys: Monkey array) (result: MonkeyResult) =
    monkeys |> Array.updateAt result.ThrowTo

let runRound (worryAfterInspection: Monkey -> int64 -> int64) (monkeys: Monkey array) (round: int) =
    (monkeys, seq { 0 .. monkeys.Length - 1 })
    ||> Seq.fold (fun monkeys currentIndex ->
        let monkey = monkeys[currentIndex]
        let results = processMonkey worryAfterInspection monkey

        (monkeys |> Array.updateAt currentIndex (monkey.MarkItemsInspected()), results)
        ||> Seq.fold (fun monkeys result ->
            monkeys
            |> Array.updateAt result.ThrowTo (monkeys[ result.ThrowTo ].AddItem result.Item)))

let monkeyBusiness (monkeys: Monkey array): int64 =
    monkeys
        |> Seq.map (fun m -> int64 m.ItemsInspected)
        |> Seq.sortDescending
        |> Seq.take 2
        |> Seq.reduce (*)

module Part1 =
    let result () =
        let worryAfterInspection _ x = x / 3L
        let finalState = (monkeys, seq { 0..19 }) ||> Seq.fold (runRound worryAfterInspection)

        finalState |> monkeyBusiness

module Part2 =
    let result () =
        let commonMultiple = monkeys |> Seq.map (fun m -> m.Test.DivisibleBy) |> Seq.reduce (*)
        printf $"COMMON MULTIPLE {commonMultiple}\n\n"
        let worryAfterInspection monkey x =
            x % commonMultiple
        let finalState = (monkeys, seq { 0..10000 - 1 }) ||> Seq.fold (runRound worryAfterInspection)

        printMonkeys finalState

        finalState |> monkeyBusiness

let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
