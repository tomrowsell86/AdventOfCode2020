// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
open Validation
open System

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

    let recordFilter record =
        let rec loop = function 
        | hd::tl -> 
            let inclusiveYearRange (raw:string) (lower:int) (upper:int) = 
                if raw.Length <> 4
                then    
                    false 
                else
                    let year = Int32.Parse(raw)
                    year >= lower && year <= upper
            let validateField = function
            | Field "byr" x -> 
                if inclusiveYearRange x 1920 2002
                then
                    loop tl
                else
                    false
            | Field "iyr" x -> 
                if inclusiveYearRange x 2010 2020
                then    
                    loop tl
                else
                    false
            | Field "eyr" x -> 
                if inclusiveYearRange x 2020 2030
                then    
                    loop tl
                else
                    false
            
            | _ -> true
            if validateField hd 
            then
                loop tl
            else
                false
        | [] -> true
        if not(List.length record = 8 || (not (List.exists (fun (s:String) -> s.StartsWith("cid")) record) && List.length record = 7))
        then
            false
        else
            loop record

    let validPassports = passportList (Seq.toList (System.IO.File.ReadLines("input.txt"))) |> List.where recordFilter

    printfn "Valid passport count %d" (List.length validPassports)
    0 // return an integer exit code