// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
  
    let passportList xs =  
        let rec getPassports xs = 
            [
                let nextEmptyIndex =
                    try                  
                        List.findIndex (fun s -> s = String.Empty) xs 
                    with
                        | :? Collections.Generic.KeyNotFoundException -> -1
                     
                if nextEmptyIndex = -1
                then
                    yield xs
                else
                    let splitList = List.splitAt nextEmptyIndex xs
                    
                    yield fst splitList
                    if not (List.isEmpty (snd splitList))
                    then
                        yield! getPassports  (List.skip 1 (snd splitList))
            ]

        getPassports xs

    let passports = passportList (Seq.toList (System.IO.File.ReadLines("input.txt")))
    

    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code