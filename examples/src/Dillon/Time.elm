module Dillon.Time exposing
    ( Time
    , errorToString
    , fieldType
    , fivePm
    , fromString
    , isBetween
    , nineAm
    , toString
    )

import Field as F exposing (Error)


type Time
    = Time Int


nineAm : Time
nineAm =
    Time (9 * 3600)


fivePm : Time
fivePm =
    Time (17 * 3600)


fromString : String -> Result Error Time
fromString =
    F.trim
        (\s ->
            --
            -- N.B. The parsing can be improved.
            --
            -- For e.g. it currently allows "001:0002".
            --
            case s |> String.split ":" |> List.map String.toInt of
                [ Just hours, Just minutes ] ->
                    if 0 <= hours && hours < 24 && 0 <= minutes && minutes < 60 then
                        Ok (Time <| hours * 3600 + 60 * minutes)

                    else
                        Err (F.validationError s)

                _ ->
                    Err (F.syntaxError s)
        )


isBetween : Time -> Time -> Time -> Bool
isBetween (Time start) (Time end) (Time time) =
    start <= time && time <= end


toString : Time -> String
toString (Time inSeconds) =
    let
        hours =
            inSeconds // 3600

        minutes =
            (inSeconds |> modBy 3600) // 60
    in
    String.padLeft 2 '0' (String.fromInt hours) ++ ":" ++ String.padLeft 2 '0' (String.fromInt minutes)


fieldType : F.Type Time
fieldType =
    F.customType
        { fromString = fromString
        , toString = toString
        }


errorToString : String -> Error -> String
errorToString name =
    F.errorToString
        { onBlank = "The " ++ name ++ " time is required."
        , onSyntaxError = \s -> "The " ++ name ++ " time is incorrectly formatted: " ++ s ++ "."
        , onValidationError = \s -> "The " ++ name ++ " time is invalid: " ++ s ++ "."
        }
