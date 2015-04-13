namespace Javi

open Microsoft.FSharp.Collections

module Sudoku =

    type CellLocation = { X:int; Y:int;}

    type CellValue = int option

    type Board = { Grid:Map<CellLocation,CellValue>; Size:int}


    let createBoardCells size = 
        seq { 
            for i in 1 .. size  do
            for j in 1 ..size 
                do yield (i,j) } |> Seq.toList
        

    let createBoard size =
        let grid = seq { 
            for i in 1 .. size  do
            for j in 1 ..size 
                do yield { X=i; Y=j}, Some j } |> Map.ofSeq
        { Grid  = grid; Size = size}


    let printBoard board = 
        let size  = board.Size
        board.Grid |> Map.iter (fun pos value->
                match pos with
                | { CellLocation.Y = y; } when y  = size -> printfn "%O" value
                | _ -> printf "%O" value) 

    let hasDuplicates seq =
        let lookup = System.Collections.Generic.HashSet<'T>() 
        Seq.exists (fun item ->  not (lookup.Add item)) seq

    let isValidRow board i =
        let rows = [1..board.Size] 
                    |> Seq.map (fun col -> board.Grid.TryFind {CellLocation.X=i;CellLocation.Y=col})
                    |> Seq.filter (fun rowOption-> rowOption.IsSome && rowOption.IsSome) 
                    |> Seq.map (fun cell->cell.Value.Value)
        // Check if all numbers are distinct
        not (hasDuplicates rows)

    let isValidColumn board i =
        let rows = [1..board.Size] 
                    |> Seq.map (fun row -> board.Grid.TryFind {CellLocation.X=row;CellLocation.Y=i})
                    |> Seq.filter (fun rowOption-> rowOption.IsSome && rowOption.IsSome) 
                    |> Seq.map (fun cell->cell.Value.Value)
        // Check if all numbers are distinct
        not (hasDuplicates rows)

    let isValidRegion board i j =
        let positions = seq{
                for x in [i..i+2] do
                    for y in [j..j+2] do
                        yield (x,y) 
            }
        let itemsInRegion = positions
                            |> Seq.map (fun (x,y) -> board.Grid.TryFind {CellLocation.X=x;CellLocation.Y=y})
                            |> Seq.filter (fun rowOption-> rowOption.IsSome && rowOption.IsSome) 
                            |> Seq.map (fun cell->cell.Value.Value)
        not (hasDuplicates itemsInRegion)

    let allRegions = 
        seq{
                for x in [0..2] do
                    for y in [0..2] do
                        yield (x*3+1, y*3+1) 
            }

    let isValidBoard board = 
        [1..board.Size] |> Seq.forall (fun i -> isValidRow board i && isValidColumn board i)
        && (allRegions |> Seq.forall(fun (x,y)-> isValidRegion board x y))


    let solve board = 
        let rec solveRec listPos board =
            seq{
                match listPos with
                | [] -> yield board
                | (x,y)::xs ->
                    let currentCell = board.Grid.TryFind {X=x; Y=y} 
                    match currentCell with
                    | None -> 
                        let validBoards = 
                                [1..board.Size] 
                                |> Seq.map (fun value -> 
                                                    let newGrid = board.Grid.Add({X=x;Y=y}, Some value)
                                                    {Grid = newGrid ; Size = board.Size}) 
                                |> Seq.filter (fun b -> isValidBoard b) 
                                |> Seq.collect (fun b-> solveRec xs b)
                        yield! validBoards
                    | Some(value) -> yield! solveRec xs board
            }
        solveRec (createBoardCells board.Size) board
            



