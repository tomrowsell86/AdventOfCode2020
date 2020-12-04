open PartOne

[<EntryPoint>]
let main argv =
    let expenses = 
        System.IO.File.ReadAllLines("input.txt")
       |> List.ofArray 
       |> List.map decimal
     
    let result = traverseList expenses

    let resultB = PartTwo.outer PartTwo.scanForSum expenses

    printfn "Final Result is %s" (resultB.ToString()) 
    0 // return an integer exit code