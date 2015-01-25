// http://en.wikipedia.org/wiki/Eight_queens_puzzle

type Board = (int*int) list

let print (board:Board) = 
    let n = List.length board
    board 
    |> List.rev 
    |> List.map (fun (i,j) -> [ for x in 0 .. n-1 do if x = j then yield "[Q] " else yield "[ ] " ]) 
    |> List.map (fun row -> List.reduce (+) row )
    |> List.reduce (fun acc elem -> acc+ "\n" + elem  )
    
let safeQueen (x,y) (board:Board) = 
    board |> List.forall (fun (i,j) -> i<>x && j<>y && abs (i-x) <> abs (j-y))
       
let findAllSolutions n =
    let rec findSolutions solutions i = 
        if i = n then
            solutions
        else
            let newsol = solutions |> List.map (
                fun sol -> [0..n-1] 
                            |> List.filter (fun pos -> safeQueen (i,pos) sol) 
                            |> List.map (fun pos -> (i,pos)::sol))
                        |> List.concat
            findSolutions newsol (i+1)
    findSolutions [[]] 0

// Depth-first so you can get n number of solutions lazily
let findAllSolutionsSeq n =
    let rec findSolutions solution i = 
        seq{
            if i = n then
                yield solution
            else
                let newsols = seq { 0 .. (n-1) }
                                |> Seq.filter (fun pos -> safeQueen (i,pos) solution) 
                                |> Seq.map (fun pos -> (i,pos)::solution)
                for sol in newsols do yield! findSolutions sol (i+1)
                
        }
    findSolutions [] 0


[<EntryPoint>]
let main argv = 
    
    let x = findAllSolutionsSeq 24 |> Seq.head

    printfn "%s" (print x)
    0 // return an integer exit code
