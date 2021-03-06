module Day10
open System 
let scanAdapters (joltages:list<int>) : (list<int>) = 
    let rec calculateHeadDiff (joltages:list<int>) accumulatedDiffs = 
        match joltages with
        | hd::adj::tl -> 
            let diff = adj - hd
            let next = calculateHeadDiff (hd::tl)
            if diff > 0 && diff <= 3
            then
                next (diff::accumulatedDiffs)
            else
                next accumulatedDiffs
        | [] | [_]-> if List.isEmpty accumulatedDiffs then 3 else (List.min accumulatedDiffs) 
                
     
    let rec composeDiffList = function
        | hd::tl ->
                   
            let next = calculateHeadDiff (hd::tl) List.empty
            next::(composeDiffList tl)
        | [] -> []
    composeDiffList (0::(List.sort joltages)) 

let calcChargingPermutations diffList = 
    let rec countPermutations counter = function
        | hd::adj::tl ->
            printfn "Head is : %d" hd
            if hd + adj < 4
            then
                countPermutations (counter + 1 ) (hd::tl)
            else
                countPermutations counter tl
        | [_] | [] -> counter
    countPermutations 0 diffList

