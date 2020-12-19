open System

let boardingPasses = Seq.toList (System.IO.File.ReadLines("input.txt"))

let rec spaceDivide (lower:int) (upper:int) lowerChar = function
    | hd::tl -> 
        if hd = lowerChar
        then
            spaceDivide lower (lower + (upper - lower)/2) lowerChar tl
        else
            spaceDivide (1 + lower + (upper - lower)/2) upper lowerChar tl
    | [] -> if lower <> upper then raise (Exception "No more instructions but more rows left!")
            else lower

let getRow (rowInstructions : list<char>) = spaceDivide 0 127 'F' rowInstructions
let getAisle (ailseInstructions: list<char>) = spaceDivide 0 7 'L' ailseInstructions 

let mapper (s : string)= 
    let instructionTuple = Array.toList (s.ToCharArray()) |> List.splitAt 7  
    getRow (fst instructionTuple) * 8 + getAisle (snd instructionTuple)
    
[<EntryPoint>]
let main _ =
    let seatIds = List.map mapper boardingPasses 
    List.iter (printfn "%d") seatIds
    printfn "Highest SeatId is %d" (List.max seatIds)
    0 // return an integer exit code