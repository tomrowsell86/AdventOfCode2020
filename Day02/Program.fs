// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
open Types
open Utils 

let rec partA = function
| hd::tl ->  
    let rec passwordMeetsPolicy (policy:Policy) (index:int) = function
    | hd::tl -> 
        if List.isEmpty tl
        then
            let isValid index = (index>= policy.Occurance.Lower && index <= policy.Occurance.Upper)

            if hd = policy.BoundLetter
            then    
                isValid (index + 1)
            else
                isValid index
        else
            if hd = policy.BoundLetter
            then
                
                if (index + 1) > policy.Occurance.Upper
                then
                    false
                else
                    passwordMeetsPolicy policy (index + 1) tl
            else
                passwordMeetsPolicy policy (index) tl
    |[] -> false

    passwordMeetsPolicy hd.Policy 0 (Array.toList (hd.Password.ToCharArray())) :: partA tl
| [] -> List.empty

    
[<EntryPoint>]
let main argv =
    let passwords = System.IO.File.ReadAllLines("input.txt") |> List.ofArray 

    let policies = passwords |> List.map parsePasswordPolicy 
    let validCount =  List.where (id) (partA policies)  
    printfn "number of valid passwords is : %d" (List.length validCount)
    0 // return an integer exit code