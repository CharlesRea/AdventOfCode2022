module Day8

open System
open System.IO

let input = File.ReadAllLines("./08/input.txt")

let width = input[0].Length
let height = input.Length

let trees =
    Seq.allPairs (seq { 0 .. width - 1 }) (seq { 0 .. height - 1 })
    |> Seq.map (fun (x, y) -> ((x, y), input[y][x] |> string |> Int32.Parse))
    |> Map.ofSeq

let neighbouringCoordinates (x: int, y: int) : ((int * int) seq) seq =
    seq {
        yield seq { x - 1 .. -1 .. 0 } |> Seq.map (fun x1 -> (x1, y))
        yield seq { x + 1 .. width - 1 } |> Seq.map (fun x1 -> (x1, y))
        yield seq { y - 1 .. -1 .. 0 } |> Seq.map (fun y1 -> (x, y1))
        yield seq { y + 1 .. height - 1 } |> Seq.map (fun y1 -> (x, y1))
    }

module Part1 =
    let isVisible (treeCoords: int * int) : bool =
        neighbouringCoordinates treeCoords
        |> Seq.exists (Common.all (fun neighbourCoords -> trees[treeCoords] > trees[neighbourCoords]))

    let result () =
        Seq.allPairs (seq { 0 .. width - 1 }) (seq { 0 .. height - 1 })
        |> Seq.filter isVisible
        |> Seq.length

module Part2 =
    let visibleInDirection (tree: int) (treesInDirection: int seq) : int =
        min
            (Seq.length treesInDirection)
            (1
             + (treesInDirection
                |> Seq.takeWhile (fun treeInDirection -> treeInDirection < tree)
                |> Seq.length))

    let scenicScore (treeCoords: int * int) : int =
        neighbouringCoordinates treeCoords
        |> Seq.map (Seq.map (fun coords -> Map.find coords trees))
        |> Seq.map (visibleInDirection (Map.find treeCoords trees))
        |> Seq.reduce (*)

    let result () =
        Seq.allPairs (seq { 0 .. width - 1 }) (seq { 0 .. height - 1 })
        |> Seq.map scenicScore
        |> Seq.max

let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
