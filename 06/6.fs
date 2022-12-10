module Day6

open System.IO

let part1 () =
    File.ReadAllText("./06//input.txt")
    |> Common.contiguousSubseqs 4
    |> Seq.map (Set.ofSeq >> Set.count)
    |> Seq.findIndex ((=) 4)
    |> (+) 4

let part2 () =
    File.ReadAllText("./06/input.txt")
    |> Common.contiguousSubseqs 14
    |> Seq.map (Set.ofSeq >> Set.count)
    |> Seq.findIndex ((=) 14)
    |> (+) 14

let solve = { PartOne = part1; PartTwo = part2 }
