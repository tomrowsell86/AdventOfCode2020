module Types

type Occurrence = { Lower: int; Upper: int }

type Policy =
    { BoundLetter: char
      Occurance: Occurrence }

type PolicyPwPair = { Policy: Policy; Password: string }
type StringSplit = { Chars: list<char>; Index: int }
