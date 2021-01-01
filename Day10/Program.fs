open System
open Day10

let joltages = IO.File.ReadLines("input.txt") |> Seq.map int |> Seq.toList 


[<EntryPoint>]
let main _ =
    let diffs = scanAdapters joltages
    let diffCounts = List.countBy id diffs 
    let getDiffsOf diff = List.find (fun t -> fst t = diff) diffCounts |> snd
    let result = getDiffsOf 1 * getDiffsOf 3
    printfn "Scan result = %d" result
    0 // return an integer exit code