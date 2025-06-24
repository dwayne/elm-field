module Example1.Data.Username exposing
    ( Username
    , errorToString
    , fieldType
    , fromString
    , toString
    )

import Field as F exposing (Error)


type Username
    = Username String


fromString : String -> Result Error Username
fromString =
    F.trim
        (\s ->
            let
                len =
                    String.length s
            in
            if len < 3 then
                Err <| F.customError "The username must have at least 3 characters."

            else if len > 25 then
                Err <| F.customError "The username must have at most 25 characters."

            else
                Ok (Username s)
        )


toString : Username -> String
toString (Username s) =
    s


fieldType : F.Type Username
fieldType =
    F.customType
        { fromString = fromString
        , toString = toString
        }


errorToString : Error -> String
errorToString =
    F.errorToString
        { onBlank = "The username is required."
        , onSyntaxError = always ""
        , onValidationError = always ""
        }
