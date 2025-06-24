module Example2.Data.PasswordConfirmation exposing
    ( CustomError(..)
    , Error
    , PasswordConfirmation
    , errorToString
    , fieldType
    , fromString
    , toString
    )

import Field.Advanced as F


type PasswordConfirmation
    = PasswordConfirmation String


type alias Error =
    F.Error CustomError


type CustomError
    = Mismatch


fromString : String -> Result Error PasswordConfirmation
fromString =
    F.nonBlankString.fromString >> Result.map PasswordConfirmation


toString : PasswordConfirmation -> String
toString (PasswordConfirmation s) =
    s


fieldType : F.Type Error PasswordConfirmation
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
        , onCustomError = always "The password confirmation does not match."
        }
