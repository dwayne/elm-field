module Test.Fixtures.Username exposing
    ( Username
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
            if String.length s < 5 then
                Err <| F.customError "Too short"

            else
                Ok <| Username s
        )


toString : Username -> String
toString (Username s) =
    s



-- FIELD


fieldType : F.Type Username
fieldType =
    F.customType
        { fromString = fromString
        , toString = toString
        }
