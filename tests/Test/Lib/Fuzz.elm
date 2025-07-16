module Test.Lib.Fuzz exposing (blankString)

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
