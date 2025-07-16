module Dillon.Role exposing
    ( Role(..)
    , errorToString
    , fieldType
    , fromString
    , toString
    )

import Field as F exposing (Error)
import Lib.String as String


type Role
    = Regular
    | Admin
    | SuperAdmin


fromString : String -> Result Error Role
fromString =
    F.trim
        (\s ->
            case s of
                "regular" ->
                    Ok Regular

                "admin" ->
                    Ok Admin

                "superAdmin" ->
                    Ok SuperAdmin

                _ ->
                    Err (F.validationError s)
        )


toString : Role -> String
toString role =
    case role of
        Regular ->
            "regular"

        Admin ->
            "admin"

        SuperAdmin ->
            "superAdmin"


fieldType : F.Type Role
fieldType =
    F.customType
        { fromString = fromString
        , toString = toString
        }


errorToString : Error -> String
errorToString =
    F.errorToString
        { onBlank = "The role is required."
        , onSyntaxError = always ""
        , onValidationError = always "The role is not valid."
        }
