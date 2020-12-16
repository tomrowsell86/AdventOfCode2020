module Validation

open System.Text.RegularExpressions

let (|Field|_|) fieldName str =
   let m = Regex(fieldName + ":(\w*)").Match(str)
   if m.Success
   then Some (List.head(List.skip 1 [ for x in m.Groups -> x.Value ]))
   else None
