// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
open System

type Occurrence = { Lower:int; Upper:int }
type Policy = { BoundLetter : char ;   Occurance : Occurrence  }
type PolicyPwPair = { Policy : Policy ; Password : string }
type StringSplit = { Chars : list<char> ; Index : int}

let splitString (s:string)(splitChar : char) =  
    let rec splitChars (a : list<char>) (splitChar : char) = 
        let rec findSplit (index : int) (accString : list<char>) = function  
        | hd::tl -> 
            if hd = splitChar
            then 
                { Index = index + 1; Chars = accString }//(index , accString)
            else   
                findSplit (index + 1) (hd::accString) tl
        | [] ->  { Index = index; Chars = accString }
        let result = findSplit 0 List.empty a
        
        if result.Index = 0
        then
            []
        else
            (String(List.toArray (List.rev result.Chars)))::(splitChars (List.skip result.Index a) splitChar)
    splitChars (List.ofArray (s.ToCharArray())) splitChar 

let parsePasswordPolicy line = 
    let parts = splitString line ' '
    let parseRange = function
    | hd :: _ ->
        let parseRange = function
        | lower::[upper]->
            Some{Lower = lower; Upper = upper}
   
        | []|_ -> None
        splitString hd '-'  |>  List.map(int) |> parseRange
    | [] -> None
    
    let range = parseRange parts
    {Password = List.last parts ; Policy = {BoundLetter = char (List.last(splitString (List.item 1 parts ) ':')) ; Occurance = range.Value } }  


 
let rec recurseValidPasswords = function
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

    passwordMeetsPolicy hd.Policy 0 (Array.toList (hd.Password.ToCharArray())) :: recurseValidPasswords tl
| [] -> List.empty

    
[<EntryPoint>]
let main argv =
    let passwords = System.IO.File.ReadAllLines("input.txt") |> List.ofArray 

    let policies = passwords |> List.map parsePasswordPolicy 
    let validCount =  List.where (id) (recurseValidPasswords policies)  
    printfn "number of valid passwords is : %d" (List.length validCount)
    0 // return an integer exit code