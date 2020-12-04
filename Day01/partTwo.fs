module PartTwo

let rec scanForThreePartProductFor2020 = function 
| hdOuter :: tlOuter -> 
    let rec secondProductScan  = function
        | hd :: adj :: tl ->
        let rec thirdProductScan  = function 
            | hd :: adj :: adj2 :: tl ->
                printfn "%s %s %s" (hd.ToString()) (adj.ToString()) (adj2.ToString())
                if hd + adj + adj2 = 2020.0m
                then    
                    hd * adj * adj2
                else 
                    thirdProductScan (hd::adj::tl)
            | _ -> 0.0m

        let result = thirdProductScan (hd::adj::tl)
        if result <> 0.0m
        then
            result
        else
            secondProductScan (hd::tl) 
        | _ -> 0.0m

    let result = secondProductScan  (hdOuter::tlOuter)
    if result <> 0.0m
    then
        result
    else
        scanForThreePartProductFor2020 tlOuter
| _ -> 0.0m
