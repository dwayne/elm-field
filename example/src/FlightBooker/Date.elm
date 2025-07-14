module FlightBooker.Date exposing
    ( Date
    , default
    , errorToString
    , fieldType
    , fromString
    , isLaterThan
    , toString
    , today
    )

import Char
import Date
import Field as F exposing (Error)
import FlightBooker.Parser as P
import Parser as P exposing ((|.), (|=))
import Task


type Date
    = Date Date.Date


default : Date
default =
    Date (Date.fromRataDie 739433)


today : (Date -> msg) -> Cmd msg
today toMsg =
    Task.perform (Date >> toMsg) Date.today


fromString : String -> Result Error Date
fromString =
    F.trim
        (\s ->
            case P.run dateParser s of
                Ok iso ->
                    case Date.fromIsoString iso of
                        Ok date ->
                            Ok (Date date)

                        Err err ->
                            Err (F.validationError err)

                Err deadEnds ->
                    Err (F.syntaxError <| P.deadEndsToString deadEnds)
        )


dateParser : P.Parser String
dateParser =
    P.succeed (\dd mm yyyy -> yyyy ++ "-" ++ mm ++ "-" ++ dd)
        |= P.chompExactly 2 Char.isDigit
        |. P.chompIf ((==) '.')
        |= P.chompExactly 2 Char.isDigit
        |. P.chompIf ((==) '.')
        |= P.chompExactly 4 Char.isDigit
        |. P.end


isLaterThan : Date -> Date -> Bool
isLaterThan (Date date1) (Date date2) =
    Date.compare date2 date1 /= LT


toString : Date -> String
toString (Date date) =
    let
        dd =
            Date.day date
                |> String.fromInt
                |> String.padLeft 2 '0'

        mm =
            Date.monthNumber date
                |> String.fromInt
                |> String.padLeft 2 '0'

        yyyy =
            Date.year date
                |> String.fromInt
                |> String.padLeft 4 '0'
    in
    dd ++ "." ++ mm ++ "." ++ yyyy



-- FIELD


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
        , onSyntaxError = identity
        , onValidationError = identity
        }
