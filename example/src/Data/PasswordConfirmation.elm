module Data.PasswordConfirmation exposing (Error(..), fieldType)

import Data.Password as Password exposing (Password)
import Field as F


type Error
    = PasswordError Password.Error
    | Mismatch


fieldType : F.Type (F.Error Error) Password
fieldType =
    F.customType
        { fromString = Password.fromString >> Result.mapError (F.mapErrorType PasswordError)
        , toString = Password.toString
        }
