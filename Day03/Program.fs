// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =

    let lines = System.IO.File.ReadAllLines("input.txt") |> List.ofArray |> List.map (fun s -> Array.toList (s.ToCharArray())) 

    let rec scanLines treeCounter colIndex = function
    | hd::tl -> 

        let getNextColIndex index =
            printfn "col index %d" index
            if index + 3 > 30
            then
                Math.Abs(index-28)
            else
                index + 3

        let treeIncrement index item =
            if  (List.item index item) = '#' 
            then
                1
            else
                0
         
        scanLines (treeCounter + (treeIncrement colIndex hd)) (getNextColIndex colIndex) tl

    | [] -> treeCounter

    let treeCount = scanLines 0 0 lines
    let message = from "F#" // Call the function
    printfn "Tree count is %d. Ouch!" treeCount
    0 // return an integer exit code