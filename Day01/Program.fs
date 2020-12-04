[<EntryPoint>]
let main argv =
    let expenses = 
        System.IO.File.ReadAllLines("input.txt")
       |> List.ofArray 
       |> List.map decimal
     
    let partOneResult = PartOne.traverseList expenses

    let partTwoResult = PartTwo.scanForThreePartProductFor2020 expenses

    printfn "Part One - Final Result is %s" (partOneResult.ToString()) 

    printfn "Part Two - Final Result is %s" (partTwoResult.ToString()) 
    0 // return an integer exit code