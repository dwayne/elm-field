module Data.Password exposing
    ( Error(..)
    , Password
    , fieldType
    , fromString
    , toString
    )

import Field as F


type Password
    = Password String


type Error
    = TooShort Int
    | MissingRequiredChars


fromString : String -> Result (F.Error Error) Password
fromString =
    F.trim
        (\s ->
            let
                n =
                    String.length s
            in
            if n < 8 then
                Err (F.CustomError <| TooShort n)

            else if hasRequiredChars s then
                Ok (Password s)

            else
                Err (F.CustomError MissingRequiredChars)
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


fieldType : F.Type (F.Error Error) Password
fieldType =
    F.customType
        { fromString = fromString
        , toString = toString
        }
