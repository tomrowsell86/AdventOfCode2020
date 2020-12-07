// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
open System


type Policy = { BoundLetter : char ;   Occurance : int  }
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





[<EntryPoint>]
let main argv =
    let passwords = System.IO.File.ReadAllLines("input.txt") |> List.ofArray

    let message = splitString "Hello.World.John.Jim" '.' 
    message |> List.iter (fun s -> printfn "%s" s) 
    0 // return an integer exit code