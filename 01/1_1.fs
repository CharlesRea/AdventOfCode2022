module Day1

open System.IO
open FSharpPlus

let partOne () =
    File.ReadAllText("./01/input.txt")
    |> String.split ["\n\n"]
    |> Seq.map (fun elf -> elf |> String.split ["\n"] |> Seq.sumBy int)
    |> Seq.max

let partTwo () =
    File.ReadAllText("./01/input.txt")
    |> String.split ["\n\n"]
    |> Seq.map (fun elf -> elf |> String.split ["\n"] |> Seq.sumBy int)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum

let solve = { PartOne = partOne; PartTwo = partTwo }