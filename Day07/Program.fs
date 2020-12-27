
open BagRule

[<EntryPoint>]
let main _ =
    let bagList = System.IO.File.ReadLines("input.txt") |> Seq.toList
    
    let filter = [0..9] |> List.map char |> List.append ['.']  
    let sanitiseBagString s = String.filter (fun c -> not(List.contains c filter)) s |> (fun a -> a.Trim())

    let rules = List.map parseRule bagList
        
    let getContainers bag rules =
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
        loop bag List.empty rules

    let getAllBagsFor container rules = 
        let rec loop container bagTally = function
        | hd::tl -> 
            if hd.Container = container
            then
                let result = List.fold (fun a b ->  a + ((snd b) + (snd b) * (loop (fst b) List.empty  rules ))) 0  hd.Bags
                printfn "Container : %s Result %d" container result
                result
            else
                loop container bagTally tl   
        | [] -> 0  
        loop container List.empty rules

    let containers = getContainers "shiny gold" rules
    let numberOfBags = getAllBagsFor "shiny gold" rules
    printfn "Part A : Number of container bags is %d" (List.distinct containers |> List.length) 
    printfn "Part B : Number of container bags is %d" numberOfBags
    0 // return an integer exit code