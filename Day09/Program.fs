
open System

let preAmbleScan (numbers : list<int64>) batchSize = 
    let rec windowLoop (numbers:list<int64>)= 
        match numbers with
        | hd::tl when List.length tl >= (batchSize ) ->
            let window = hd::List.take (batchSize-1) tl
            let nextNumber = List.skip (batchSize-1) tl |> List.head
            
            let rec scanLoopOuter targetSum = function
            | hd::tl ->
                let rec scanLoopInner  = function
                | hd::adj::tl -> 
                    if hd + adj <> targetSum
                    then
                        printfn "%d, %d" hd adj
                        scanLoopInner (hd::tl)
                    else
                        Some(targetSum)

                | _ -> None
                let result = scanLoopInner (hd::tl) 
                if result <> None
                then
                    result
                else
                    scanLoopOuter nextNumber tl
            | _ -> None
            let result = scanLoopOuter nextNumber  window 
            if result <> None
            then
                printfn "*****************************************" 
                windowLoop tl
            else
                Some(nextNumber)

                
        | _ -> None
    windowLoop numbers

     

[<EntryPoint>]
let main _ =

    let lines = IO.File.ReadLines("input.txt") |> Seq.map int64  |> Seq.toList
    
    
    let result = preAmbleScan lines 25

    let partB lines (targetSum: int64) = 
        let rec loop acc buffer = function
            |hd::tl -> 
                if hd <> targetSum
                then
                    let currentAcc = (hd + acc)
                    if currentAcc = targetSum
                    then
                        let allProducts = (hd::buffer)
                        List.min allProducts + List.max allProducts
                    else if currentAcc < targetSum
                    then
                        loop currentAcc (buffer@[hd]) tl
                    else
                        let nextTl buffer = 
                            if not(List.isEmpty buffer)
                            then
                                (List.skip 1 buffer@[hd]@tl)
                            else
                                tl
                        let tl' = nextTl buffer
                        loop 0L List.empty tl'
                else
                    loop 0L List.empty tl
            | _ -> raise (exn "no products found for ")
        loop 0L List.empty lines
    let partBResult = partB lines result.Value
    printfn "Part A Value is %d" result.Value
    printfn "Part B Value is %d" partBResult
    0 // return an integer exit code