// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
  
    let passportList xs =  
        let rec getPassports xs = 

            let getLineFields (s:String) = Array.toList (s.Split(' ', StringSplitOptions.RemoveEmptyEntries))
            let getBlockFields fields = List.collect getLineFields fields
            [
                let nextEmptyIndex =
                    try                  
                        List.findIndex (fun s -> s = String.Empty) xs 
                    with
                        | :? Collections.Generic.KeyNotFoundException -> -1
                     
                if nextEmptyIndex = -1
                then
                    yield getBlockFields xs
                else
                    let splitList = List.splitAt nextEmptyIndex xs
                    

                    let fields = fst splitList |> getBlockFields 

                    yield fields 
                    if not (List.isEmpty (snd splitList))
                    then
                        yield! getPassports  (List.skip 1 (snd splitList))
            ]

        getPassports xs
    let validFilter fields = List.length fields = 8 || (not (List.exists (fun (s:String) -> s.StartsWith("cid")) fields) && List.length fields = 7)
    let validPassports = passportList (Seq.toList (System.IO.File.ReadLines("input.txt"))) |> List.where validFilter


    let message = from "F#" // Call the function
    printfn "Valid passport count %d" (List.length validPassports)
    0 // return an integer exit code