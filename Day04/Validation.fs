module Validation

open System
open System.Text.RegularExpressions

let (|Field|_|) fieldName str =
    let m = Regex(fieldName + ":([a-zA-Z0-9#]*)").Match(str)
    if m.Success
    then Some(List.head (List.skip 1 [ for x in m.Groups -> x.Value ]))
    else None

let (|Inches|Metric|None|) str =
    let m = Regex("(\d+)(cm|in)").Match(str)
    if m.Success then
        let matches =
            List.skip 1 ([ for x in m.Groups -> x.Value ])

        let value = System.Int32.Parse(List.head matches)

        if List.last matches = "cm" then Metric(value) else Inches(value)
    else
        None

let (|Colour|None|) str =
    if Regex("#[0-9a-fA-F]{6}$").IsMatch(str) then Colour else None

let validateField =
    let inclusiveYearRange (raw: string) (lower: int) (upper: int) =
        if raw.Length <> 4 then
            false
        else
            let year = Int32.Parse(raw)
            year >= lower && year <= upper

    function
    | Field "byr" x -> inclusiveYearRange x 1920 2002
    | Field "iyr" x -> inclusiveYearRange x 2010 2020
    | Field "eyr" x -> inclusiveYearRange x 2020 2030
    | Field "hgt" x ->
        let validate height =
            match height with
            | Metric height -> height >= 150 && height <= 193
            | Inches height -> height >= 59 && height <= 76
            | None -> false
            | _ -> false

        validate x
    | Field "hcl" x ->
        match x with
        | Colour -> true
        | None -> false
    | Field "ecl" x ->
        match x with
        | "amb"
        | "blu"
        | "brn"
        | "gry"
        | "grn"
        | "hzl"
        | "oth" -> true
        | _ -> false
    | Field "pid" x -> Regex("^\d{9}$").IsMatch(x)
    | Field "cid" _ -> true
    | _ -> false
