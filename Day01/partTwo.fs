module PartTwo
let rec scanForSum  = function 
    | hd :: adj :: adj2 :: tl ->
        printfn "%s %s %s" (hd.ToString()) (adj.ToString()) (adj2.ToString())
        if hd + adj + adj2 = 2020.0m
        then    
            hd * adj * adj2
        else 
            scanForSum (hd::adj::tl)
    | _ -> 0.0m

let rec outer fn = function 
| hdOuter :: tlOuter -> 
    let rec traverseList fn = function
        | hd :: adj :: tl ->
            let result = fn (hd::adj::tl)
            if result <> 0.0m
            then
                result
            else
                traverseList fn (hd::tl) 
        | hd -> 0.0m
    let result = traverseList fn (hdOuter::tlOuter)
    if result <> 0.0m
    then
        result
    else
        outer fn tlOuter
| _ -> 0.0m
