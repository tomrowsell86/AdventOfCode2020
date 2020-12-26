// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open BagRule

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    let bagList = System.IO.File.ReadLines("input.txt") |> Seq.toList
    
    let filter = [0..9] |> List.map char |> List.append ['.']  
    let sanitiseBagString s = String.filter (fun c -> not(List.contains c filter)) s |> (fun a -> a.Trim())

    let rules = List.map parseRule bagList
        
    let rec loop bag containers = function
    | hd::tl -> 
        if  List.contains bag (List.map fst hd.Bags) 
        then
            hd.Container::containers
                @loop hd.Container List.empty rules
                @loop bag containers tl
        else
            loop bag containers tl
    | [] -> containers

    let containers = loop "shiny gold" List.empty rules
    printfn "Number of container bags is %d" (List.distinct containers |> List.length) 

    0 // return an integer exit code