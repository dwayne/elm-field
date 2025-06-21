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
    , optional
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
    , processed : Validation (Error e a) a
    }


type Raw
    = Initial String
    | Dirty String



-- TYPE


type alias Type e a =
    { fromString : String -> Result (Error e a) a
    , fromValue : a -> Result (Error e a) a
    , toString : a -> String
    }


int : Type e Int
int =
    customInt Ok


nonNegativeInt : Type e Int
nonNegativeInt =
    customInt
        (\n ->
            if n >= 0 then
                Ok n

            else
                Err (ValidationError n)
        )


positiveInt : Type e Int
positiveInt =
    customInt
        (\n ->
            if n > 0 then
                Ok n

            else
                Err (ValidationError n)
        )


customInt : (Int -> Result (Error e Int) Int) -> Type e Int
customInt validate =
    { fromString =
        trim
            >> Result.andThen
                (\t ->
                    case String.toInt t of
                        Just n ->
                            validate n

                        Nothing ->
                            Err (ParseError t)
                )
    , fromValue = validate
    , toString = String.fromInt
    }


trim : String -> Result (Error e a) String
trim s =
    let
        t =
            String.trim s
    in
    if String.isEmpty t then
        Err Required

    else
        Ok t


optional : Type e a -> Type e (Maybe a)
optional tipe =
    { fromString =
        \s ->
            case tipe.fromString s of
                Ok value ->
                    Ok (Just value)

                Err Required ->
                    Ok Nothing

                Err (ParseError t) ->
                    Err (ParseError t)

                Err (ValidationError value) ->
                    --
                    -- N.B. The unfortunate consequence is that we MUST use a `Maybe a`
                    -- even though we want it to NEVER be `Nothing`.
                    --
                    Err (ValidationError (Just value))

                Err (CustomError e) ->
                    Err (CustomError e)
    , fromValue =
        \maybeValue ->
            case maybeValue of
                Just v1 ->
                    case tipe.fromValue v1 of
                        Ok v2 ->
                            Ok (Just v2)

                        Err Required ->
                            Ok Nothing

                        Err (ParseError s) ->
                            Err (ParseError s)

                        Err (ValidationError value) ->
                            --
                            -- N.B. The unfortunate consequence is that we MUST use a `Maybe a`
                            -- even though we want it to NEVER be `Nothing`.
                            --
                            Err (ValidationError (Just value))

                        Err (CustomError e) ->
                            Err (CustomError e)

                Nothing ->
                    Ok Nothing
    , toString = Maybe.map tipe.toString >> Maybe.withDefault ""
    }



-- ERROR


type Error e a
    = Required
    | ParseError String
    | ValidationError a
    | CustomError e



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
    let
        g error =
            case error of
                Required ->
                    Required

                ParseError s ->
                    ParseError s

                ValidationError a ->
                    ValidationError a

                CustomError x ->
                    CustomError (f x)
    in
    Field
        { fromString = tipe.fromString >> Result.mapError g
        , fromValue = tipe.fromValue >> Result.mapError g
        , toString = tipe.toString
        }
        { raw = state.raw
        , processed = V.mapError g state.processed
        }
