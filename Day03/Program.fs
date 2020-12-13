// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =

    let lines = System.IO.File.ReadAllLines("input.txt") |> List.ofArray |> List.map (fun s -> Array.toList (s.ToCharArray())) 

    let rec scanLines (treeCounter) (colIndex) (horizontalOffset) = function
    | hd::tl -> 

        let getNextColIndex index offset =
         //   printfn "col index %d" index
            if index + offset > 30
            then
                Math.Abs(index + offset - 31)
            else
                index + offset

        let treeIncrement index item =
            if  (List.item index item) = '#' 
            then
                1
            else
                0
         
        scanLines (treeCounter + (treeIncrement colIndex hd)) (getNextColIndex colIndex horizontalOffset) horizontalOffset tl

    | [] -> int64 treeCounter

    let allVerticals horizontalOffset = scanLines 0 0 horizontalOffset lines

    let rec skipLines inc = function    
    | hd::tl -> 
        if (List.length tl) - (inc-1) >= 0
        then 
            hd::skipLines 2 (List.skip (inc - 1) tl)
        else
            [hd]
    
    | [] -> List.empty

    let partB =  int64 (Seq.fold (*) (int64 1) (Seq.map allVerticals {1..2..7})) *  (scanLines 0 0 1 (skipLines 2 lines))
    let message = from "F#" // Call the function
    printfn "Part A: Tree count is %d. Ouch!" (allVerticals 3)
    printfn "Part B: Tree count is %d. Ouch!" (partB)
    
    0 // return an integer exit code