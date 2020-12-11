// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
open Types
open Utils 

let rec scanPasswords (policyCheckFn : Policy -> 'a->bool) (seedState:'a)  (nextOnMatchedState : int -> 'a -> 'a ) = function
| hd::tl ->  
    let rec passwordMeetsPolicy (policy:Policy) (index:int) (state :('a)) = function
    | hd::tl -> 
        if List.isEmpty tl
        then

            if hd = policy.BoundLetter
            then    
                policyCheckFn policy (nextOnMatchedState index state)
            else
                policyCheckFn policy state
        else
            let nextIndex = index + 1
            if hd = policy.BoundLetter
            then
                passwordMeetsPolicy policy nextIndex (nextOnMatchedState index state) tl
            else
                passwordMeetsPolicy policy nextIndex state tl
                
    |[] -> false

    passwordMeetsPolicy hd.Policy 0 seedState (Array.toList (hd.Password.ToCharArray())) :: scanPasswords policyCheckFn seedState nextOnMatchedState tl
| [] -> List.empty

[<EntryPoint>]
let main argv =
    let passwords = System.IO.File.ReadAllLines("input.txt") |> List.ofArray 

    let partAPolicyCheck p state = (state>= p.Occurance.Lower && state <= p.Occurance.Upper)
    let partBMatchesRule p index = p.Occurance.Lower = index || p.Occurance.Upper = index  
    
    let partBPolicyCheck (p:Policy) (state : list<int>) = List.where (fun index -> p.Occurance.Lower = index + 1 || p.Occurance.Upper = index  + 1) state |> List.length = 1
    let policies = passwords |> List.map parsePasswordPolicy 
    
    let partAOnMatchedLetter = (fun _ state -> state + 1)
    let partBOnMatchedLetter index state = index::state

    let validCountPartA =  List.where (id) (scanPasswords partAPolicyCheck 0 partAOnMatchedLetter policies)  
    let validCountPartB =  List.where (id) (scanPasswords partBPolicyCheck List.empty partBOnMatchedLetter policies)  
    
    printfn "Part a : number of valid passwords is : %d" (List.length validCountPartA)
    printfn "Part b : number of valid passwords is : %d" (List.length validCountPartB)
    
    0 // return an integer exit code