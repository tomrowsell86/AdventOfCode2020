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

let mapper (s : string) : (int*int*int)= 
    let instructionTuple = Array.toList (s.ToCharArray()) |> List.splitAt 7  
    let getSeatNumbers (row,aisle) = (row, aisle, row * 8 + aisle )
    let row = getRow (fst instructionTuple) 
    let aisle = getAisle (snd instructionTuple)
    (row, aisle, row * 8 + aisle)
    
[<EntryPoint>]
let main _ =
    let seatIds = List.map mapper boardingPasses 
               |> List.sortBy (fun (_,_,seatNo) -> seatNo) 
    let (_,_,maxSeatNumber) = (List.maxBy (fun(_,_,seatNumber) -> seatNumber) seatIds)

    let findGap (seatNumbers : list<int>) = 
        let rec loop last = function 
        | hd::tl -> if hd - last > 1
                    then
                        hd - 1
                    else
                        loop hd tl
        | [] -> raise (Exception "No missing seats found")
        loop (List.head seatNumbers) (List.skip 1 seatNumbers)

    List.iter (fun (r,a,n) -> (printfn "%d %d %d") r a n) seatIds

    let yourMissingSeat = List.map (fun (_,_,seatId) -> seatId) seatIds |>  findGap

    printfn "Highest SeatId is %d" maxSeatNumber
    printfn "Your missing seat is %d" yourMissingSeat
    0 