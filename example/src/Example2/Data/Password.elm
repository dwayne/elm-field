module Example2.Data.Password exposing
    ( CustomError(..)
    , Error
    , Password
    , errorToString
    , fieldType
    , fromString
    , toString
    )

import Field.Advanced as F


type Password
    = Password String


type alias Error =
    F.Error CustomError


type CustomError
    = TooShort Int
    | MissingRequiredChars


fromString : String -> Result Error Password
fromString =
    F.trim
        (\s ->
            let
                n =
                    String.length s
            in
            if n < 8 then
                Err (F.customError <| TooShort n)

            else if hasRequiredChars s then
                Ok (Password s)

            else
                Err (F.customError MissingRequiredChars)
        )


hasRequiredChars : String -> Bool
hasRequiredChars s =
    let
        isSpecial ch =
            ch == '!' || ch == '@' || ch == '#' || ch == '$' || ch == '%' || ch == '^' || ch == '&' || ch == '*'
    in
    List.all identity
        [ String.any Char.isUpper s
        , String.any Char.isLower s
        , String.any Char.isDigit s
        , String.any isSpecial s
        ]


toString : Password -> String
toString (Password s) =
    s


fieldType : F.Type Error Password
fieldType =
    F.customType
        { fromString = fromString
        , toString = toString
        }


errorToString : Error -> String
errorToString =
    F.errorToString
        { onBlank = "The password is required."
        , onSyntaxError = always ""
        , onValidationError = always ""
        , onCustomError =
            \e ->
                case e of
                    TooShort _ ->
                        "The password must have at least 8 characters."

                    MissingRequiredChars ->
                        "The password must contain at least 1 of each of the following: a lowercase character, an uppercase character, a number, and a special character in the set \"(!@#$%^&*)\"."
        }
