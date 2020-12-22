// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

let parseGroups (rawFileLines : list<string>) groupSelector =
           
    let rec loop (currentGroups : list<string>) = function
    | hd::tl -> 
        [
            if hd = String.Empty
            then
                
                yield List.length (groupSelector currentGroups)
                yield! loop list.Empty tl
            else
                yield! loop (hd::currentGroups) tl
        ]
    |[] -> [ yield List.length (groupSelector currentGroups) ]
    loop List.Empty rawFileLines 
              
[<EntryPoint>]
let main argv =
    let fileLines = Seq.toList (System.IO.File.ReadLines("input.txt")) 
    
    let splitToChars (s:String) =  Array.toList (s.ToCharArray())

    let partAGroupSelector group = List.collect splitToChars group |> List.distinct

    let partBGroupSelector group = List.collect splitToChars group |> List.groupBy (id) |> 
                                    List.where (fun g -> List.length (snd g) = List.length group)

    let partAGroupAnswerTotal = List.sum (parseGroups fileLines partAGroupSelector)
    let partBGroupAnswerTotal = List.sum (parseGroups fileLines partBGroupSelector)

    printfn "Part a distinct answer sum is %d" partAGroupAnswerTotal
    printfn "Part b answer is %d" partBGroupAnswerTotal

    0 // return an integer exit code