// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
open Validation
open System

[<EntryPoint>]
let main _ =
  
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

    let dumpRecord (record:list<string>) = 
        let join a b =  match b with 
                        | "eyr" -> a + " " + b 
                        | _ -> a
        printfn "%s" (List.fold join "" (List.sort record))

    let recordFilter record =
        let rec loop = function 
        | hd::tl -> 
            if validateField hd 
            then
                loop tl
            else
                false
        | [] -> true
        loop record

    let passports = passportList (Seq.toList (System.IO.File.ReadLines("input.txt")))  |> 
                    List.where (fun record -> List.length record = 8 || (not (List.exists (fun (s:String) -> s.StartsWith("cid")) record) && List.length record = 7))
    let validPassportsPartB = passports |> List.where recordFilter
    List.iter dumpRecord validPassportsPartB
    printfn "Part a full fields passport count : %d " (List.length passports)
    printfn "Valid passport count %d" (List.length validPassportsPartB)
    0 // return an integer exit code