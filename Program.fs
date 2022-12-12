open Argu
open System

type Part =
    | One = 1
    | Two = 2
    | All = 3

type Arguments =
    | [<MainCommand; ExactlyOnce>] Day of int
    | [<Unique; AltCommandLine("-p")>] Part of Part

    interface IArgParserTemplate with
        member arg.Usage =
            match arg with
            | Day _ -> "day to run"
            | Part _ -> "one, two or all"

[<EntryPoint>]
let main argv =
    let errorHandler =
        ProcessExiter(
            colorizer =
                function
                | ErrorCode.HelpText -> None
                | _ -> Some ConsoleColor.Red
        )

    let parser =
        ArgumentParser.Create<Arguments>(programName = "advent-of-code", errorHandler = errorHandler)

    let results = parser.ParseCommandLine argv

    let day = results.GetResult Day
    let part = results.GetResult(Part, defaultValue = Part.All)

    let print (solver: Solver<'one, 'two>) =
        match part with
        | Part.One -> printf $"{solver.PartOne()}\n"
        | Part.Two -> printf $"{solver.PartTwo()}\n"
        | Part.All ->
            printf $"Part one:\n {solver.PartOne()}\n\n"
            printf $"Part two:\n {solver.PartTwo()}\n"
        | _ -> failwith "invalid part"

    match day with
    | 1 -> print Day1.solve
    | 2 -> print Day2.solve
    | 3 -> print Day3.solve
    | 4 -> print Day4.solve
    | 5 -> print Day5.solve
    | 6 -> print Day6.solve
    | 7 -> print Day7.solve
    | 8 -> print Day8.solve
    | 9 -> print Day9.solve
    | 10 -> print Day10.solve
    | 11 -> print Day11.solve
    | 12 -> print Day12.solve
    | _ -> failwith "Invalid day"

    0
