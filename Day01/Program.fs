open System



let rec scanForSum  = function 
    | hd :: adj :: tl ->
        if hd + adj = 2020.0m
        then    
            hd * adj
        else 
            scanForSum (hd::tl)
    | _ -> 0.0m
    
let rec listWalk = function
    | hd :: tl ->
        let result = scanForSum (hd::tl)
        if result <> 0.0m
        then
            result
        else
            listWalk tl
    | hd -> 0.0m

[<EntryPoint>]
let main argv =
    let expenses = 
        System.IO.File.ReadAllLines("input.txt")
       |> List.ofArray 
       |> List.map decimal
     
    let result = listWalk expenses



    printfn "Final Result is %s" (result.ToString()) 
    0 // return an integer exit code