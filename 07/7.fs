module Day7

open System.IO
open Common
open FSharpPlus

let input = File.ReadLines("./07/input.txt")

type File = { Name: string; Size: int64 }

type Cd =
    | Dir of string
    | Up
    | Root

type TerminalLine =
    | Dir of string
    | File of File
    | Cd of Cd
    | Ls

let parseOutputLine (line: string) : TerminalLine =
    match line with
    | ParseRegex @"\$ cd (.+)" [ dir ] ->
        match dir with
        | "/" -> TerminalLine.Cd Root
        | ".." -> TerminalLine.Cd Up
        | dir -> TerminalLine.Cd(Cd.Dir dir)
    | ParseRegex @"\$ ls" _ -> TerminalLine.Ls
    | ParseRegex @"dir (.+)" [ dir ] -> TerminalLine.Dir dir
    | ParseRegex @"(\d+) (.+)" [ Int64 size; file ] -> TerminalLine.File { Name = file; Size = size }
    | _ -> failwith $"Invalid terminal line: {line}"

let terminalLines = input |> Seq.map parseOutputLine

let combinePaths path1 path2 =
    path1
    + (if (String.endsWith "/" path1) || (String.startsWith "/" path2) then
           ""
       else
           "/")
    + path2

type Directory =
    { Parent: Directory option
      Name: string
      mutable Size: int64
      mutable Files: Set<string> }

    static member Root: Directory =
        { Parent = None
          Name = "/"
          Size = 0
          Files = Set.empty }

    member this.Path() : string =
        let parentPath =
            this.Parent
            |> Option.map (fun parent -> parent.Path())
            |> Option.defaultValue ""

        combinePaths parentPath this.Name

    member this.AddFile(file: File) =
        match this.Files |> Set.contains file.Name with
        | true -> ()
        | false ->
            this.Files <- this.Files |> Set.add file.Name
            this.IncreaseSize(file.Size)

    member private this.IncreaseSize(size: int64) =
        this.Size <- this.Size + size
        this.Parent |> Option.map (fun parent -> parent.IncreaseSize size) |> ignore

type State =
    { Path: string
      DirectoriesByPath: Map<string, Directory> }

    member this.Directory() : Directory =
        this.DirectoriesByPath |> Map.find this.Path

let processLines (lines: TerminalLine seq): Map<string, Directory> =
    let initialState =
        { Path = "/"
          DirectoriesByPath = Map.empty |> Map.add "/" Directory.Root }

    let fold (state: State) (line: TerminalLine) : State =
        match line with
        | Dir dirName ->
            let dirPath = combinePaths state.Path dirName

            let directory =
                Map.tryFind dirPath state.DirectoriesByPath
                |> Option.defaultValue (
                    { Parent = Some(state.DirectoriesByPath |> Map.find state.Path)
                      Name = dirName
                      Size = 0
                      Files = Set.empty }
                )

            { state with DirectoriesByPath = Map.add dirPath directory state.DirectoriesByPath }

        | File file ->
            state.Directory().AddFile file
            state

        | Cd(Cd.Dir dirName) ->
            let path = combinePaths state.Path dirName

            let directory =
                Map.tryFind path state.DirectoriesByPath
                |> Option.defaultValue (
                    { Parent = Some(state.Directory())
                      Name = dirName
                      Size = 0
                      Files = Set.empty }
                )

            { state with
                Path = path
                DirectoriesByPath = Map.add path directory state.DirectoriesByPath }

        | Cd Up ->
            let parent =
                match state.Directory().Parent with
                | Some parent -> parent
                | None -> failwith "cd .. at root"

            { state with Path = parent.Path() }

        | Cd Root -> { state with Path = "/" }

        | Ls -> state

    let finalState = Seq.fold fold initialState lines
    finalState.DirectoriesByPath

let directories = processLines terminalLines

let part1 () =
        directories
        |> Map.values
        |> Seq.map (fun directory -> directory.Size)
        |> Seq.filter (fun size -> size <= 100000L)
        |> Seq.sum

let part2 () =
    let rootDirectory = directories |> Map.find "/"
    let freeSpace = 70000000L - rootDirectory.Size
    let spaceToDelete = 30000000L - freeSpace
    directories
    |> Map.values
    |> Seq.map (fun directory -> directory.Size)
    |> Seq.filter (fun size -> size >= spaceToDelete)
    |> Seq.min

let solve =
    { PartOne = part1
      PartTwo = part2 }
