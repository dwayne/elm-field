module Dillon.Date exposing
    ( Date
    , add
    , errorToString
    , fieldType
    , fromString
    , isAfter
    , jul1st2025
    , nights
    , toString
    , today
    )

import Date
import Field as F exposing (Error)
import Task


type Date
    = Date Date.Date


jul1st2025 : Date
jul1st2025 =
    Date (Date.fromRataDie 739433)


today : (Date -> msg) -> Cmd msg
today toMsg =
    Task.perform (Date >> toMsg) Date.today


fromString : String -> Result Error Date
fromString =
    F.trim
        (\s ->
            case Date.fromIsoString s of
                Ok date ->
                    Ok (Date date)

                Err err ->
                    Err (F.validationError err)
        )


isAfter : Date -> Date -> Bool
isAfter (Date first) (Date second) =
    --
    -- Returns True iff the second date comes after the first date.
    --
    Date.compare first second == LT


add : Int -> Date -> Date
add nDays (Date date) =
    Date (Date.fromRataDie (Date.toRataDie date + nDays))


nights : Date -> Date -> Int
nights (Date first) (Date second) =
    --
    -- Returns the number of nights between the dates.
    --
    -- It assumes first <= second.
    --
    max 0 (Date.toRataDie second - Date.toRataDie first)


toString : Date -> String
toString (Date date) =
    Date.toIsoString date


fieldType : F.Type Date
fieldType =
    F.customType
        { fromString = fromString
        , toString = toString
        }


errorToString : String -> Error -> String
errorToString name =
    F.errorToString
        { onBlank = "The " ++ name ++ " date is required."
        , onSyntaxError = always ""
        , onValidationError = \err -> "The " ++ name ++ " date is invalid: " ++ err ++ "."
        }
