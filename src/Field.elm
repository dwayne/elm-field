module Field exposing
    ( Error(..)
    , Field
    , Type
    , bool
    , char
    , false
    , float
    , fromString
    , fromValue
    , int
    , mapError
    , nonEmptyString
    , nonNegativeFloat
    , nonNegativeInt
    , optional
    , positiveFloat
    , positiveInt
    , setFromString
    , setFromValue
    , string
    , subsetOfChar
    , subsetOfFloat
    , subsetOfInt
    , subsetOfNonEmptyString
    , subsetOfString
    , trim
    , true
    )

import Validation as V exposing (Validation)


type Field e a
    = Field (Type e a) (State e a)


type alias State e a =
    { raw : Raw
    , processed : Validation (Error e) a
    }


type Raw
    = Initial String
    | Dirty String



-- TYPE


type alias Type e a =
    { fromString : String -> Result (Error e) a
    , fromValue : a -> Result (Error e) a
    , toString : a -> String
    }


int : Type e Int
int =
    customInt Ok


nonNegativeInt : Type e Int
nonNegativeInt =
    subsetOfInt ((<=) 0)


positiveInt : Type e Int
positiveInt =
    subsetOfInt ((<) 0)


subsetOfInt : (Int -> Bool) -> Type e Int
subsetOfInt isGood =
    customInt
        (\n ->
            if isGood n then
                Ok n

            else
                Err (ValidationError (String.fromInt n))
        )


customInt : (Int -> Result (Error e) Int) -> Type e Int
customInt validate =
    { fromString =
        trim
            >> Result.andThen
                (\t ->
                    case String.toInt t of
                        Just n ->
                            validate n

                        Nothing ->
                            Err (FormatError t)
                )
    , fromValue = validate
    , toString = String.fromInt
    }


float : Type e Float
float =
    customFloat Ok


nonNegativeFloat : Type e Float
nonNegativeFloat =
    subsetOfFloat ((<=) 0)


positiveFloat : Type e Float
positiveFloat =
    subsetOfFloat ((<) 0)


subsetOfFloat : (Float -> Bool) -> Type e Float
subsetOfFloat isGood =
    customFloat
        (\f ->
            if isGood f then
                Ok f

            else
                Err (ValidationError (String.fromFloat f))
        )


customFloat : (Float -> Result (Error e) Float) -> Type e Float
customFloat validate =
    { fromString =
        trim
            >> Result.andThen
                (\t ->
                    case String.toFloat t of
                        Just f ->
                            validate f

                        Nothing ->
                            Err (FormatError t)
                )
    , fromValue = validate
    , toString = String.fromFloat
    }


bool : Type e Bool
bool =
    { fromString =
        \s ->
            let
                t =
                    String.trim s
            in
            if String.isEmpty t then
                Ok False

            else
                Ok True
    , fromValue = Ok
    , toString = boolToString
    }


true : Type e Bool
true =
    { fromString =
        \s ->
            if String.isEmpty (String.trim s) then
                Err (ValidationError s)

            else
                Ok True
    , fromValue =
        \b ->
            if b then
                Ok True

            else
                Err (ValidationError (boolToString b))
    , toString = boolToString
    }


false : Type e Bool
false =
    { fromString =
        \s ->
            if String.isEmpty (String.trim s) then
                Ok False

            else
                Err (ValidationError s)
    , fromValue =
        \b ->
            if b then
                Err (ValidationError (boolToString b))

            else
                Ok False
    , toString = boolToString
    }


boolToString : Bool -> String
boolToString b =
    if b then
        "True"

    else
        "False"


char : Type e Char
char =
    subsetOfChar (always True)


subsetOfChar : (Char -> Bool) -> Type e Char
subsetOfChar isGood =
    let
        validate ch =
            if isGood ch then
                Ok ch

            else
                Err (ValidationError (String.fromChar ch))
    in
    { fromString =
        \s ->
            case String.uncons s of
                Just ( ch, "" ) ->
                    validate ch

                Just _ ->
                    Err (ValidationError s)

                Nothing ->
                    Err Required
    , fromValue = validate
    , toString = String.fromChar
    }


optional : Type e a -> Type e (Maybe a)
optional tipe =
    --
    -- I don't think it would be wise to use optional with string fields.
    --
    { fromString =
        \s ->
            case tipe.fromString s of
                Ok value ->
                    Ok (Just value)

                Err Required ->
                    Ok Nothing

                Err err ->
                    Err err
    , fromValue =
        \maybeValue ->
            case maybeValue of
                Just v1 ->
                    case tipe.fromValue v1 of
                        Ok v2 ->
                            Ok (Just v2)

                        Err Required ->
                            Ok Nothing

                        Err err ->
                            Err err

                Nothing ->
                    Ok Nothing
    , toString = Maybe.map tipe.toString >> Maybe.withDefault ""
    }


string : Type e String
string =
    customString (String.trim >> Ok)


nonEmptyString : Type e String
nonEmptyString =
    customString trim


subsetOfString : (String -> Result e String) -> Type e String
subsetOfString validate =
    customString (validate >> Result.mapError CustomError)


subsetOfNonEmptyString : (String -> Result e String) -> Type e String
subsetOfNonEmptyString validate =
    customString (trim >> Result.andThen (validate >> Result.mapError CustomError))


customString : (String -> Result (Error e) String) -> Type e String
customString validate =
    { fromString = validate
    , fromValue = validate
    , toString = identity
    }


trim : String -> Result (Error e) String
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


type Error e
    = Required
    | FormatError String
    | ValidationError String
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

                FormatError s ->
                    FormatError s

                ValidationError s ->
                    ValidationError s

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
