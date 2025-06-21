module Field exposing
    ( Error(..)
    , Field
    , Type
    , customInt
    , fromString
    , fromValue
    , int
    , mapError
    , nonNegativeInt
    , positiveInt
    , setFromString
    , setFromValue
    , trim
    )

import Validation as V exposing (Validation)


type Field e a
    = Field (Type e a) (State e a)


type alias State e a =
    { raw : Raw
    , processed : Validation e a
    }


type Raw
    = Initial String
    | Dirty String



-- TYPE


type alias Type e a =
    { fromString : String -> Result e a
    , fromValue : a -> Result e a
    , toString : a -> String
    }


int : Type Error Int
int =
    customInt Ok


nonNegativeInt : Type Error Int
nonNegativeInt =
    customInt
        (\n ->
            if n >= 0 then
                Ok n

            else
                Err ValidationError
        )


positiveInt : Type Error Int
positiveInt =
    customInt
        (\n ->
            if n > 0 then
                Ok n

            else
                Err ValidationError
        )


customInt : (Int -> Result Error Int) -> Type Error Int
customInt validate =
    { fromString = trim >> Result.andThen (String.toInt >> Result.fromMaybe ParseError >> Result.andThen validate)
    , fromValue = validate
    , toString = String.fromInt
    }


trim : String -> Result Error String
trim s =
    let
        t =
            String.trim s
    in
    if String.isEmpty t then
        Err Required

    else
        Ok t



-- ERROR


type Error
    = Required
    | ParseError
    | ValidationError



-- CONSTRUCT


fromString : Type e a -> String -> Field e a
fromString tipe s =
    Field
        tipe
        { raw = Initial s
        , processed = V.fromResult (tipe.fromString s)
        }


fromValue : Type e a -> a -> Field e a
fromValue tipe value =
    Field
        tipe
        { raw = Initial (tipe.toString value)
        , processed = V.fromResult (tipe.fromValue value)
        }



-- CHANGE


setFromString : String -> Field e a -> Field e a
setFromString s (Field tipe state) =
    Field
        tipe
        { raw = Dirty s
        , processed = V.fromResult (tipe.fromString s)
        }


setFromValue : a -> Field e a -> Field e a
setFromValue value (Field tipe state) =
    Field
        tipe
        { raw = Dirty (tipe.toString value)
        , processed = V.fromResult (tipe.fromValue value)
        }



-- MAP


mapError : (x -> y) -> Field x a -> Field y a
mapError f (Field tipe state) =
    Field
        { fromString = tipe.fromString >> Result.mapError f
        , fromValue = tipe.fromValue >> Result.mapError f
        , toString = tipe.toString
        }
        { raw = state.raw
        , processed = V.mapError f state.processed
        }
