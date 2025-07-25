module Example2.PasswordConfirmation exposing
    ( CustomError(..)
    , Error
    , PasswordConfirmation
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
    (F.typeToConversion F.nonBlankString).fromString >> Result.map PasswordConfirmation


toString : PasswordConfirmation -> String
toString (PasswordConfirmation s) =
    s


fieldType : F.Type Error PasswordConfirmation
fieldType =
    F.customType
        { fromString = fromString
        , toString = toString
        }
