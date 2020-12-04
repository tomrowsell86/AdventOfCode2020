module PartOne
let rec scanForSum  = function 
    | hd :: adj :: tl ->
        if hd + adj = 2020.0m
        then    
            hd * adj
        else 
            scanForSum (hd::tl)
    | _ -> 0.0m
    
let rec traverseList = function
    | hd :: tl ->
        let result = scanForSum (hd::tl)
        if result <> 0.0m
        then
            result
        else
            traverseList tl
    | hd -> 0.0m
