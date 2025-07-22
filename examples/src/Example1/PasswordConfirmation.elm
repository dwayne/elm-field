module Example1.PasswordConfirmation exposing
    ( PasswordConfirmation
    , errorToString
    , fieldType
    , fromString
    , toString
    )

import Field as F exposing (Error)


type PasswordConfirmation
    = PasswordConfirmation String


fromString : String -> Result Error PasswordConfirmation
fromString =
    --
    -- Notice how I reuse fromString from the nonBlankString type.
    --
    F.nonBlankString.fromString >> Result.map PasswordConfirmation


toString : PasswordConfirmation -> String
toString (PasswordConfirmation s) =
    s


fieldType : F.Type PasswordConfirmation
fieldType =
    F.customType
        { fromString = fromString
        , toString = toString
        }


errorToString : Error -> String
errorToString =
    F.errorToString
        { onBlank = "The password confirmation is required."
        , onSyntaxError = always ""
        , onValidationError = always ""
        }
