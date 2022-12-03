#r @"nuget: FSharpPlus"

open System.IO
open FSharpPlus

let result =
    File.ReadAllText("./1_input.txt")
    |> String.split ["\n\n"]
    |> Seq.map (fun elf -> elf |> String.split ["\n"] |> Seq.sumBy int)
    |> Seq.max

printf $"%d{result}"