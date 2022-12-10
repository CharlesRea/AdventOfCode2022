[<Microsoft.FSharp.Core.AutoOpen>]
module Solver

type Solver<'one, 'two> = {
    PartOne: unit -> 'one;
    PartTwo: unit -> 'two;
}
