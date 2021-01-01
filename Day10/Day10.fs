module Day10
open System 
let scanAdapters (joltages:list<int>) : (List<int>) = 
    let rec scanLoopOuter = function
        | hd::tl ->
            let rec scanLoopInner (joltages:list<int>) diffs = 
                match joltages with
                | hd::adj::tl -> 
                    let diff = adj - hd
                    let next = scanLoopInner (hd::tl)
                    if diff > 0 && diff <= 3
                    then
                        next (diff::diffs)
                    else
                        next diffs
                | [] | [_]-> 
                        if List.isEmpty diffs then -99999 else List.min diffs
                    
            let next = scanLoopInner (hd::tl) List.empty
            next::(scanLoopOuter tl)
        | [] -> []
    3::(scanLoopOuter (0::(List.sort joltages))) 

