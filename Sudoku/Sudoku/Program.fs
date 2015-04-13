// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open Javi.Sudoku

[<EntryPoint>]
let main argv =

    let board = createBoard 9

    printBoard board

    printfn "Valid row 1: %O" (isValidRow board 1)

    printfn "Valid col 1: %O" (isValidColumn board 1)

    let solution = solve {Grid=Map.empty;Size=9} |> Seq.head

    printBoard solution

    printfn "Valid region in solution 1: %O" (isValidRegion solution 1 1) 

    printfn "Valid region in board 1: %O" (isValidRegion board 1 1) 

    0 // return an integer exit code
