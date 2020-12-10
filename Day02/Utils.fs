module Utils
open System
open Types

let splitString (s: string) (splitChar: char) =
    let rec splitChars (a: list<char>) (splitChar: char) =
        let rec findSplit (index: int) (accString: list<char>) =
            function
            | hd :: tl ->
                if hd = splitChar
                then { Index = index + 1; Chars = accString } //(index , accString)
                else findSplit (index + 1) (hd :: accString) tl
            | [] -> { Index = index; Chars = accString }

        let result = findSplit 0 List.empty a

        if result.Index = 0 then
            []
        else
            (String(List.toArray (List.rev result.Chars)))
            :: (splitChars (List.skip result.Index a) splitChar)

    splitChars (List.ofArray (s.ToCharArray())) splitChar

let parsePasswordPolicy line =
    let parts = splitString line ' '

    let parseRange =
        function
        | hd :: _ ->
            let parseRange =
                function
                | lower :: [ upper ] -> Some { Lower = lower; Upper = upper }

                | []
                | _ -> None

            splitString hd '-' |> List.map (int) |> parseRange
        | [] -> None

    let range = parseRange parts
    { Password = List.last parts
      Policy =
          { BoundLetter = char (List.last (splitString (List.item 1 parts) ':'))
            Occurance = range.Value } }
