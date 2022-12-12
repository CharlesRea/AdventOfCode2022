module Day12

open System.IO

let input = File.ReadAllLines("./12/input.txt")

type Location = { Height: int; Location: int * int }

let parseHeight =
    function
    | 'E' -> int 'z'
    | 'S' -> int 'a'
    | c -> int c

let coords =
    Seq.allPairs (seq { 0 .. input[0].Length - 1 }) (seq { 0 .. input.Length - 1 })

let startPoint = coords |> Seq.find (fun (x, y) -> input[y][x] = 'S')
let endPoint = coords |> Seq.find (fun (x, y) -> input[y][x] = 'E')
let endLocation = { Location = endPoint; Height = parseHeight 'E' }

let locations =
    coords
    |> Seq.map (fun (x, y) ->
        { Location = (x, y)
          Height = input[y][x] |> parseHeight })
    |> Seq.sortByDescending (fun l -> l.Height)
    |> Seq.filter (fun l -> l.Location <> endPoint)
    |> Seq.toList

type State =
    { Unsolved: Map<int * int, Location>
      LastSolved: Location seq
      LastDistance: int
      Solved: Map<int * int, int> }

let shortestDistances: Map<int*int, int> =
    let unsolvedNeighboursWhichCanAccess
        (state: State)
        ({ Height = height; Location = (x, y) }: Location)
        : Location seq =
        [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
        |> Seq.map (fun neighbour -> state.Unsolved |> Map.tryFind neighbour)
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
        |> Seq.filter (fun neighbour -> neighbour.Height >= height - 1)

    let rec solveRec (state: State) : Map<int*int, int> =
        let solved =
            state.LastSolved
            |> Seq.collect (unsolvedNeighboursWhichCanAccess state)
            |> Seq.distinct
            |> Seq.toList

        let nextState =
            { Unsolved =
                (state.Unsolved, solved)
                ||> Seq.fold (fun unsolved location -> Map.remove location.Location unsolved)
              LastSolved = solved
              LastDistance = state.LastDistance + 1
              Solved =
                (state.Solved, solved)
                ||> Seq.fold (fun solved location ->
                    Map.add location.Location (state.LastDistance + 1) solved) }

        match solved with
        | [] -> nextState.Solved
        | _ -> solveRec nextState

    let initialState =
        { Unsolved = locations |> Seq.map (fun location -> location.Location, location) |> Map.ofSeq
          LastSolved = Seq.singleton endLocation
          LastDistance = 0
          Solved = Map.empty |> Map.add endPoint 0 }

    solveRec initialState

module Part1 =
    let result () =
        shortestDistances
        |> Map.find startPoint

module Part2 =
    let result () =
        locations
        |> Seq.filter (fun l -> l.Height = parseHeight 'a')
        |> Seq.map (fun l -> shortestDistances |> Map.tryFind l.Location)
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
        |> Seq.min

let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
