module Data.Email exposing
    ( Email
    , fieldType
    , fromString
    , toString
    )

import Field as F


type Email
    = Email String


fromString : String -> Result (F.Error ()) Email
fromString =
    F.trim
        (\s ->
            if String.contains "@" s then
                Ok (Email s)

            else
                Err (F.ValidationError s)
        )


toString : Email -> String
toString (Email s) =
    s


fieldType : F.Type (F.Error ()) Email
fieldType =
    F.customType
        { fromString = fromString
        , toString = toString
        }
