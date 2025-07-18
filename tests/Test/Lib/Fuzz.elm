module Test.Lib.Fuzz exposing (blankString, nonSingleCharString)

import Fuzz exposing (Fuzzer)


blankString : Fuzzer String
blankString =
    let
        toWhitespace c =
            if Char.isAlpha c then
                ' '

            else
                '\t'
    in
    Fuzz.asciiString
        |> Fuzz.map (String.map toWhitespace)


nonSingleCharString : Fuzzer String
nonSingleCharString =
    let
        toNonSingleCharString s =
            if String.length s == 1 then
                String.cons 'a' s

            else
                s
    in
    Fuzz.asciiString
        |> Fuzz.map toNonSingleCharString
