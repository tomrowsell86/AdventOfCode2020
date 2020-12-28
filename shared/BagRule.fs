module BagRule
open System.Text.RegularExpressions

type BagRule =
    { Container: string
      Bags: list<string * int> }


let parseRule (line: string) =
    let parts = line.Split(" contain ")
    let containedBags = function
    | Regex "(\d+)\s(\w+\s+\w+)\sbag" [quantity; colour] -> 
        Some(colour,System.Int32.Parse(quantity))
    | _ -> None
    
    let parseContainer name = match name with 
                                | Regex "(\w+\s+\w+)\sbag" [colour] -> colour
                                | _ -> raise (exn "Invalid Data") 
                                
    { Container = parseContainer (Array.head parts)
      Bags =
          parts
           |> Array.last
           |> fun s ->
               s.Split(", ")
               |> Array.map containedBags
               |> Array.where (fun b -> b.IsSome)
               |> Array.map (fun b -> b.Value)
               |> Array.toList
    }
