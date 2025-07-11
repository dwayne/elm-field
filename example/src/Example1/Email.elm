module Example1.Email exposing
    ( Email
    , errorToString
    , fieldType
    , fromString
    , toString
    )

import Field as F exposing (Error)


type Email
    = Email String


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


fieldType : F.Type Email
fieldType =
    F.customType
        { fromString = fromString
        , toString = toString
        }


errorToString : Error -> String
errorToString =
    F.errorToString
        { onBlank = "The email is required."
        , onSyntaxError = always "The email is not valid."
        , onValidationError = always ""
        }
