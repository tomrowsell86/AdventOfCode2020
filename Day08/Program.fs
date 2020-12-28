open Shared.Regex
open System


let initProcessing lines = 
    let rec loop behind ahead acc executedCommands = 

        let cmdId behindCount = 90000 - behindCount

        let nextCommand acc offset (ahead : list<string>) (behind : list<string>) current= 
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
                acc
            else
                match hd with
                | Regex "([a-z]{3})\s((\+|\-)\d+)" [command; arg; _] -> 
                    let operand = Int32.Parse(arg)
                   

                    match command with
                    | "nop" -> nextCommand acc 1 tl behind  hd (cmdId (List.length behind)::executedCommands)
                    | "acc" -> nextCommand (acc + operand) 1 tl behind hd (cmdId (List.length behind)::executedCommands) 
                    | "jmp" -> nextCommand acc operand tl behind hd (cmdId (List.length behind)::executedCommands)
                    | _ -> raise (exn "Unrecognised command")
                | _ -> raise (exn "Input error")
        | [] -> acc
    loop List.empty lines  0 List.empty

[<EntryPoint>]
let main argv =
    let lines = System.IO.File.ReadLines("input.txt") |> Seq.toList
    let result = initProcessing lines

    printfn "Part A result is : %d" result
    0 // return an integer exit code