module Data.Username exposing
    ( Error(..)
    , Username
    , fieldType
    , fromString
    , toString
    )

import Field as F


type Username
    = Username String


type Error
    = TooShort Int
    | TooLong Int


fromString : String -> Result (F.Error Error) Username
fromString =
    F.trim
        (\s ->
            let
                len =
                    String.length s
            in
            if len < 3 then
                Err (F.CustomError <| TooShort len)

            else if len > 25 then
                Err (F.CustomError <| TooLong len)

            else
                Ok (Username s)
        )


toString : Username -> String
toString (Username s) =
    s


fieldType : F.Type (F.Error Error) Username
fieldType =
    F.customType
        { fromString = fromString
        , toString = toString
        }
