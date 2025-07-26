module Positive exposing (fieldType, fromString, toString)

import Field.Advanced as F exposing (Error, Type)


type Positive
    = Positive Int


fromString : String -> Result (Error e) Positive
fromString =
    (F.typeToConverters F.positiveInt).fromString >> Result.map Positive


toString : Positive -> String
toString (Positive p) =
    (F.typeToConverters F.positiveInt).toString p


fieldType : Type (Error e) Positive
fieldType =
    F.customType
        { fromString = fromString
        , toString = toString
        }
