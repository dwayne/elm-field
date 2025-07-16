module Example2.Password exposing
    ( CustomError(..)
    , Error
    , Password
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
    | MissingRequiredChars String


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
                Err (F.customError <| MissingRequiredChars "!@#$%^&*")
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
