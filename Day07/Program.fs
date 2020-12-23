// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom
type BagRule = {Container:string;Bags : list<string>}
[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    let bagList = System.IO.File.ReadLines("input.txt") |> Seq.toList
    let sanitiseBagString s = String.filter (fun c -> c <> '.') s
    let parseRule (line:string) = 
        let parts = line.Split("contains")
        {
            Container =  (parts |> Array.head); 
            Bags = (parts |> Array.last |> fun s -> s.Split(',') |> Array.toList |> List.map sanitiseBagString) 
         }
    let rules = List.map parseRule bagList
        
    let rec loop bag containers = function
    | hd::tl -> 
        if  List.contains bag hd.Bags 
        then
            loop hd.Container (hd.Container::containers) rules
        else
            loop bag containers tl
    | [] -> containers

    let containers = loop "shiny gold bag" List.empty rules
    printfn "Number of container bags is %d" (List.length containers) 



    printfn "Hello world %s" message
    0 // return an integer exit code