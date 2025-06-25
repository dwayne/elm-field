module Example2.Data.Email exposing
    ( Email
    , Error
    , fieldType
    , fromString
    , toString
    )

import Field.Advanced as F


type Email
    = Email String


type alias Error =
    F.Error Never


fromString : String -> Result Error Email
fromString =
    F.trim
        (\s ->
            if String.contains "@" s then
                Ok (Email s)

            else
                Err (F.syntaxError s)
        )


toString : Email -> String
toString (Email s) =
    s


fieldType : F.Type Error Email
fieldType =
    F.customType
        { fromString = fromString
        , toString = toString
        }
