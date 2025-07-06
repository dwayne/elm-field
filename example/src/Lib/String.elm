module Lib.String exposing (toCamelCase)


toCamelCase : String -> String
toCamelCase =
    let
        camelize i s =
            if i == 0 then
                String.toLower s

            else
                case String.uncons s of
                    Just ( ch, t ) ->
                        String.fromChar (Char.toUpper ch) ++ String.toLower t

                    Nothing ->
                        s
    in
    String.words >> List.indexedMap camelize >> String.concat
