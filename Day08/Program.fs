open Shared.Regex
open System


let parseCommandLine line  =
    match line with
    | Regex "([a-z]{3})\s((\+|\-)\d+)" [command; arg; _] -> 
        let operand = Int32.Parse(arg) 
        Some(command,operand)
    | _ -> None

let initProcessing lines = 
    let rec loop behind ahead acc executedCommands = 

        let cmdId behindCount = 90000 - behindCount

        let nextCommand acc offset (ahead : list<string*int>) (behind : list<string*int>) current= 
            if offset < 0
            then
                let next = (List.skip (List.length behind - (Math.Abs(offset))) behind)@[current]@ahead
                let before = List.take (List.length behind - (Math.Abs(offset))) behind
                loop before next acc
            else
                let next =  List.skip (offset - 1) ahead
                let before = behind@[current]@(List.take (offset-1) ahead)
                loop before next acc 
                
        match ahead with 
        | hd::tl -> 
            if List.contains (cmdId(List.length behind)) executedCommands
            then
                (acc,false)
            else
                let command = fst hd
                let operand = snd hd
                match command with
                | "nop" -> nextCommand acc 1 tl behind  hd (cmdId (List.length behind)::executedCommands)
                | "acc" -> nextCommand (acc + operand) 1 tl behind hd (cmdId (List.length behind)::executedCommands) 
                | "jmp" -> nextCommand acc operand tl behind hd (cmdId (List.length behind)::executedCommands)
                | _ -> raise (exn "Unrecognised command")
        | [] -> (acc,true)
    loop List.empty lines  0 List.empty

[<EntryPoint>]
let main argv =
    let parsedCommands = IO.File.ReadLines("input.txt") 
                            |> Seq.map parseCommandLine
                            |> Seq.where (fun c -> c <> None) 
                            |> Seq.map (fun c -> c.Value) 
                            |> Seq.toList
    let (partA,_) = initProcessing parsedCommands
    let partB commands =
        let rec loop behind ahead = 
            match ahead with
            | hd::tl ->
                let next = loop (behind@[hd])
                let replaced = 
                    match fst hd with 
                    | "jmp" -> ("nop",snd hd)
                    | "nop" when snd hd <> 0 ->  ("jmp", snd hd)
                    | _ -> hd
                if replaced <> hd
                then
                    let nextCmdCandidate = behind@[replaced]@(List.skip 1 ahead)
                    let (acc, terminatedOnNoMoreInstructions) = initProcessing nextCmdCandidate
                    if terminatedOnNoMoreInstructions
                    then acc
                    else next tl
                else
                    next tl
            | [] -> raise (exn "Could not find instruction change to cause natural termination")

        loop List.empty commands    
    printfn "Part A result is : %d" partA
    printfn "Part B result is : %d" (partB parsedCommands)
    0 // return an integer exit code