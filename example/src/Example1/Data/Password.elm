module Example1.Data.Password exposing
    ( Password
    , errorToString
    , fieldType
    , fromString
    , toString
    )

import Field as F exposing (Error)


type Password
    = Password String


fromString : String -> Result Error Password
fromString =
    F.trim
        (\s ->
            let
                n =
                    String.length s
            in
            if n < 8 then
                Err <| F.customError "The password must have at least 8 characters."

            else if hasRequiredChars s then
                Ok <| Password s

            else
                Err <| F.customError "The password must contain at least 1 of each of the following: a lowercase character, an uppercase character, a number, and a special character in the set \"(!@#$%^&*)\"."
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


fieldType : F.Type Password
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
        }
