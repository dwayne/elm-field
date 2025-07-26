module Field.Advanced exposing
    ( Field
    , Type
    , int, nonNegativeInt, positiveInt, nonPositiveInt, negativeInt, subsetOfInt, customSubsetOfInt
    , float, nonNegativeFloat, positiveFloat, nonPositiveFloat, negativeFloat, subsetOfFloat, customSubsetOfFloat
    , bool, true, false, customSubsetOfBool
    , CustomBoolOptions, defaultCustomBoolOptions, defaultTruthy, defaultFalsy, defaultBoolToString
    , char, subsetOfChar, customSubsetOfChar
    , string, subsetOfString, customSubsetOfString, customString
    , nonEmptyString, subsetOfNonEmptyString, customSubsetOfNonEmptyString, customNonEmptyString
    , nonBlankString, subsetOfNonBlankString, customSubsetOfNonBlankString, customNonBlankString
    , subsetOfType, customSubsetOfType, customType
    , optional, customOptional
    , empty, fromString, fromValue
    , setFromString, setFromValue, setError, setErrors, setCustomError, setCustomErrors
    , isEmpty, isNonEmpty, isBlank, isNonBlank, isClean, isDirty, isValid, isInvalid
    , toRawString, toString, toMaybe, toResult, toType
    , State, toState
    , Validation, toValidation
    , Converters, typeToConverters, toConverters
    , applyMaybe, applyResult
    , validate2, validate3, validate4, validate5
    , get, and, withDefault, andMaybe, andResult, andFinally
    , trim, customTrim
    , Error, blankError, syntaxError, validationError, customError, errorToString, mapErrorType
    , mapTypeError
    , mapError
    , fail, failWithErrors
    , prependError, appendError
    , firstError, lastError, allErrors
    )

{-|


# Field

@docs Field


# Type

@docs Type


# Primitive

Elm's primitive types: `Int`, `Float`, `Bool`, `Char`, and `String`, all have corresponding field types.


# Int

@docs int, nonNegativeInt, positiveInt, nonPositiveInt, negativeInt, subsetOfInt, customSubsetOfInt


# Float

@docs float, nonNegativeFloat, positiveFloat, nonPositiveFloat, negativeFloat, subsetOfFloat, customSubsetOfFloat


# Bool

@docs bool, true, false, customSubsetOfBool
@docs CustomBoolOptions, defaultCustomBoolOptions, defaultTruthy, defaultFalsy, defaultBoolToString


# Char

@docs char, subsetOfChar, customSubsetOfChar


# String

TODO: Explain about empty and blank strings.

@docs string, subsetOfString, customSubsetOfString, customString
@docs nonEmptyString, subsetOfNonEmptyString, customSubsetOfNonEmptyString, customNonEmptyString
@docs nonBlankString, subsetOfNonBlankString, customSubsetOfNonBlankString, customNonBlankString


# User-defined

@docs subsetOfType, customSubsetOfType, customType


# Optional

@docs optional, customOptional


# Construct

@docs empty, fromString, fromValue


# Change

@docs setFromString, setFromValue, setError, setErrors, setCustomError, setCustomErrors


# Query

@docs isEmpty, isNonEmpty, isBlank, isNonBlank, isClean, isDirty, isValid, isInvalid


# Convert

@docs toRawString, toString, toMaybe, toResult, toType


# State

@docs State, toState


# Validation

@docs Validation, toValidation


# Converters

@docs Converters, typeToConverters, toConverters


# Applicative

@docs applyMaybe, applyResult
@docs validate2, validate3, validate4, validate5
@docs get, and, withDefault, andMaybe, andResult, andFinally


# Helpers

@docs trim, customTrim


# Error

@docs Error, blankError, syntaxError, validationError, customError, errorToString, mapErrorType


# Handle Errors

@docs mapTypeError
@docs mapError
@docs fail, failWithErrors
@docs prependError, appendError
@docs firstError, lastError, allErrors

-}

import Set exposing (Set)
import Validation as V



-- FIELD


{-| It knows how to go from a `String` to a type `a` and back to a `String`. Any errors, `e`, can be accumulated over time.
-}
type Field e a
    = Field (Converters e a) (State e a)


{-| The internal state of a `Field`.

`raw` contains the unprocessed string.

`processed` contains the result of parsing `raw`. `processed` is a `Validation` so that any errors can be accumulated over time.

`clean` indicates whether or not the `Field` is considered **clean** or **dirty**. A `Field` is **clean** if it has never been changed,
via one of `setFromString`, `setFromValue`, `setError`, `setErrors`, `setCustomError`, or `setCustomErrors`, after initially creating it.
Otherwise, the `Field` is considered to be **dirty**.

-}
type alias State e a =
    { raw : String
    , processed : Validation e a
    , clean : Bool
    }


{-| Re-export the `Validation` type for convenience.
-}
type alias Validation e a =
    V.Validation e a



-- TYPE


{-| A representation for the type of a `Field`. It determines how a `String` is converted to a type `a` and back to a `String`.
Conversion is error prone, so any errors that are encountered during conversion are of type `e`.
-}
type Type e a
    = Type (Converters e a)


{-| The collection of conversion functions that comprise a `Type`. It's useful for building new field types
from existing field types.

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

-}
type alias Converters e a =
    { fromString : String -> Result e a
    , fromValue : a -> Result e a
    , toString : a -> String
    }



-- TYPE: INT


{-| Any `Int`.
-}
int : Type (Error e) Int
int =
    subsetOfInt (always True)


{-| Any `Int`, `n`, such that `n >= 0`.
-}
nonNegativeInt : Type (Error e) Int
nonNegativeInt =
    subsetOfInt ((<=) 0)


{-| Any `Int`, `n`, such that `n > 0`.
-}
positiveInt : Type (Error e) Int
positiveInt =
    subsetOfInt ((<) 0)


{-| Any `Int`, `n`, such that `n <= 0`.
-}
nonPositiveInt : Type (Error e) Int
nonPositiveInt =
    subsetOfInt ((>=) 0)


{-| Any `Int`, `n`, such that `n < 0`.
-}
negativeInt : Type (Error e) Int
negativeInt =
    subsetOfInt ((>) 0)


{-| Any `Int`, `n`, such that `isGood n` is `True`.
-}
subsetOfInt : (Int -> Bool) -> Type (Error e) Int
subsetOfInt =
    customSubsetOfInt
        { blank = Blank
        , syntaxError = SyntaxError
        , validationError = ValidationError
        }


{-| Similar to `subsetOfInt` but you get to customize the errors.
-}
customSubsetOfInt :
    { blank : e
    , syntaxError : String -> e
    , validationError : String -> e
    }
    -> (Int -> Bool)
    -> Type e Int
customSubsetOfInt errors isGood =
    customInt
        { blank = errors.blank
        , syntaxError = errors.syntaxError
        }
        (\n ->
            if isGood n then
                Ok n

            else
                Err (errors.validationError <| String.fromInt n)
        )


customInt :
    { blank : e
    , syntaxError : String -> e
    }
    -> (Int -> Result e Int)
    -> Type e Int
customInt errors validate =
    Type
        { fromString =
            customTrim errors.blank
                (\s ->
                    case String.toInt s of
                        Just n ->
                            validate n

                        Nothing ->
                            Err (errors.syntaxError s)
                )
        , fromValue = validate
        , toString = String.fromInt
        }



-- TYPE: FLOAT


{-| Any `Float`.
-}
float : Type (Error e) Float
float =
    subsetOfFloat (always True)


{-| Any `Float`, `f`, such that `f >= 0`.
-}
nonNegativeFloat : Type (Error e) Float
nonNegativeFloat =
    subsetOfFloat ((<=) 0)


{-| Any `Float`, `f`, such that `f > 0`.
-}
positiveFloat : Type (Error e) Float
positiveFloat =
    subsetOfFloat ((<) 0)


{-| Any `Float`, `f`, such that `f <= 0`.
-}
nonPositiveFloat : Type (Error e) Float
nonPositiveFloat =
    subsetOfFloat ((>=) 0)


{-| Any `Float`, `f`, such that `f < 0`.
-}
negativeFloat : Type (Error e) Float
negativeFloat =
    subsetOfFloat ((>) 0)


{-| Any `Float`, `f`, such that `isGood f` is `True`.
-}
subsetOfFloat : (Float -> Bool) -> Type (Error e) Float
subsetOfFloat =
    customSubsetOfFloat
        { blank = Blank
        , syntaxError = SyntaxError
        , validationError = ValidationError << String.fromFloat
        }


{-| Similar to `subsetOfFloat` but you get to customize the errors.
-}
customSubsetOfFloat :
    { blank : e
    , syntaxError : String -> e
    , validationError : Float -> e
    }
    -> (Float -> Bool)
    -> Type e Float
customSubsetOfFloat errors isGood =
    customFloat
        { blank = errors.blank
        , syntaxError = errors.syntaxError
        }
        (\f ->
            if isGood f then
                Ok f

            else
                Err (errors.validationError f)
        )


customFloat :
    { blank : e
    , syntaxError : String -> e
    }
    -> (Float -> Result e Float)
    -> Type e Float
customFloat errors validate =
    Type
        { fromString =
            customTrim errors.blank
                (\s ->
                    case String.toFloat s of
                        Just f ->
                            validate f

                        Nothing ->
                            Err (errors.syntaxError s)
                )
        , fromValue = validate
        , toString = String.fromFloat
        }



-- TYPE: BOOL


{-| -}
bool : Type (Error e) Bool
bool =
    subsetOfBool (always True)


{-| -}
true : Type (Error e) Bool
true =
    subsetOfBool ((==) True)


{-| -}
false : Type (Error e) Bool
false =
    subsetOfBool ((==) False)


subsetOfBool : (Bool -> Bool) -> Type (Error e) Bool
subsetOfBool =
    customSubsetOfBool
        { blank = Blank
        , syntaxError = SyntaxError
        , validationError = ValidationError
        }
        defaultCustomBoolOptions


{-| -}
type alias CustomBoolOptions =
    { truthy : Set String
    , falsy : Set String
    , toString : Bool -> String
    , caseSensitive : Bool
    }


{-| -}
defaultCustomBoolOptions : CustomBoolOptions
defaultCustomBoolOptions =
    { truthy = defaultTruthy
    , falsy = defaultFalsy
    , toString = defaultBoolToString
    , caseSensitive = False
    }


{-| -}
defaultTruthy : Set String
defaultTruthy =
    Set.fromList
        [ "true"
        , "1"
        , "yes"
        , "on"
        , "y"
        , "enabled"
        ]


{-| -}
defaultFalsy : Set String
defaultFalsy =
    Set.fromList
        [ "false"
        , "0"
        , "no"
        , "off"
        , "n"
        , "disabled"
        ]


{-| -}
defaultBoolToString : Bool -> String
defaultBoolToString b =
    if b then
        "true"

    else
        "false"


{-| -}
customSubsetOfBool :
    { blank : e
    , syntaxError : String -> e
    , validationError : String -> e
    }
    -> CustomBoolOptions
    -> (Bool -> Bool)
    -> Type e Bool
customSubsetOfBool errors options isGood =
    customBool
        { blank = errors.blank
        , syntaxError = errors.syntaxError
        }
        options
        (\b ->
            if isGood b then
                Ok b

            else
                Err (errors.validationError <| options.toString b)
        )


customBool :
    { blank : e
    , syntaxError : String -> e
    }
    -> CustomBoolOptions
    -> (Bool -> Result e Bool)
    -> Type e Bool
customBool errors options validate =
    Type
        { fromString =
            customTrim errors.blank
                (\s ->
                    let
                        t =
                            if options.caseSensitive then
                                s

                            else
                                String.toLower s
                    in
                    if Set.member t options.truthy then
                        validate True

                    else if Set.member t options.falsy then
                        validate False

                    else
                        Err (errors.syntaxError s)
                )
        , fromValue = validate
        , toString = options.toString
        }



-- TYPE: CHAR


{-| -}
char : Type (Error e) Char
char =
    subsetOfChar (always True)


{-| -}
subsetOfChar : (Char -> Bool) -> Type (Error e) Char
subsetOfChar =
    customSubsetOfChar
        { blank = Blank
        , syntaxError = SyntaxError
        , validationError = ValidationError << String.fromChar
        }


{-| -}
customSubsetOfChar :
    { blank : e
    , syntaxError : String -> e
    , validationError : Char -> e
    }
    -> (Char -> Bool)
    -> Type e Char
customSubsetOfChar errors isGood =
    let
        validate ch =
            if isGood ch then
                Ok ch

            else
                Err (errors.validationError ch)
    in
    Type
        { fromString =
            \s ->
                case String.uncons s of
                    Just ( ch, "" ) ->
                        validate ch

                    Just _ ->
                        Err (errors.syntaxError s)

                    Nothing ->
                        Err errors.blank
        , fromValue = validate
        , toString = String.fromChar
        }



-- TYPE: STRING


{-| -}
string : Type Never String
string =
    --
    -- Any string you give it is trimmed and returned.
    --
    customString (String.trim >> Ok)



--
-- Empty string -> ""
-- Blank string -> "", " ", "  ", " \t ", "\n \t \r"
--
-- Blank includes the empty string as well as strings consisting entirely of whitespace characters.
--


{-| -}
nonEmptyString : Type (Error e) String
nonEmptyString =
    --
    -- Any string you give it is left alone. The only string it doesn't accept is the empty string.
    --
    -- If it trimmed the string then the non-empty blank strings
    -- would coincide with the blank string, which we don't want.
    --
    -- This type makes a distinction between empty, non-empty blank strings, and non-blank strings.
    --
    customNonEmptyString Blank Ok


{-| -}
subsetOfNonEmptyString : (String -> Bool) -> Type (Error e) String
subsetOfNonEmptyString =
    customSubsetOfNonEmptyString
        { blank = Blank
        , validationError = ValidationError
        }


{-| -}
customSubsetOfNonEmptyString :
    { blank : e
    , validationError : String -> e
    }
    -> (String -> Bool)
    -> Type e String
customSubsetOfNonEmptyString errors =
    customNonEmptyString errors.blank << customValidateStringWith errors.validationError


{-| -}
customNonEmptyString : e -> (String -> Result e String) -> Type e String
customNonEmptyString blank validate =
    customString
        (\s ->
            if String.isEmpty s then
                Err blank

            else
                validate s
        )


{-| -}
nonBlankString : Type (Error e) String
nonBlankString =
    customString (trim Ok)


{-| -}
subsetOfNonBlankString : (String -> Bool) -> Type (Error e) String
subsetOfNonBlankString =
    customSubsetOfNonBlankString
        { blank = Blank
        , validationError = ValidationError
        }


{-| -}
customSubsetOfNonBlankString :
    { blank : e
    , validationError : String -> e
    }
    -> (String -> Bool)
    -> Type e String
customSubsetOfNonBlankString errors =
    customNonBlankString errors.blank << customValidateStringWith errors.validationError


{-| -}
customNonBlankString : e -> (String -> Result e String) -> Type e String
customNonBlankString blank validate =
    customString (customTrim blank validate)


{-| -}
subsetOfString : (String -> Bool) -> Type (Error e) String
subsetOfString =
    customSubsetOfString ValidationError


{-| -}
customSubsetOfString : (String -> e) -> (String -> Bool) -> Type e String
customSubsetOfString toValidationError =
    customString << customValidateStringWith toValidationError


validateStringWith : (String -> Bool) -> String -> Result (Error e) String
validateStringWith =
    customValidateStringWith ValidationError


customValidateStringWith : (String -> e) -> (String -> Bool) -> String -> Result e String
customValidateStringWith toValidationError isGood s =
    if isGood s then
        Ok s

    else
        Err (toValidationError s)


{-| -}
customString : (String -> Result e String) -> Type e String
customString validate =
    Type
        { fromString = validate
        , fromValue = validate
        , toString = identity
        }



-- TYPE: USER-DEFINED


{-| -}
subsetOfType : (a -> Bool) -> Type (Error e) a -> Type (Error e) a
subsetOfType =
    customSubsetOfType ValidationError


{-| -}
customSubsetOfType : (String -> e) -> (a -> Bool) -> Type e a -> Type e a
customSubsetOfType toValidationError isGood (Type converters) =
    let
        validate x =
            if isGood x then
                Ok x

            else
                Err (toValidationError <| converters.toString x)
    in
    Type
        { fromString = converters.fromString >> Result.andThen validate
        , fromValue = converters.fromValue >> Result.andThen validate
        , toString = converters.toString
        }


{-| -}
customType :
    { fromString : String -> Result e a
    , toString : a -> String
    }
    -> Type e a
customType options =
    Type
        { fromString = options.fromString
        , fromValue = Ok
        , toString = options.toString
        }



-- TYPE: OPTIONAL


{-| -}
optional : Type (Error e) a -> Type (Error e) (Maybe a)
optional =
    --
    -- I don't think it would be wise to use optional with string fields.
    --
    customOptional ((==) Blank)


{-| -}
customOptional : (e -> Bool) -> Type e a -> Type e (Maybe a)
customOptional isBlankError (Type converters) =
    Type
        { fromString =
            \s ->
                case converters.fromString s of
                    Ok value ->
                        Ok (Just value)

                    Err err ->
                        if isBlankError err then
                            Ok Nothing

                        else
                            Err err
        , fromValue =
            \maybeValue ->
                case maybeValue of
                    Just v1 ->
                        case converters.fromValue v1 of
                            Ok v2 ->
                                Ok (Just v2)

                            Err err ->
                                if isBlankError err then
                                    Ok Nothing

                                else
                                    Err err

                    Nothing ->
                        Ok Nothing
        , toString = Maybe.map converters.toString >> Maybe.withDefault ""
        }



-- TYPE: CONVERT


{-| -}
typeToConverters : Type e a -> Converters e a
typeToConverters (Type converters) =
    converters



-- CONSTRUCT


{-| -}
empty : Type e a -> Field e a
empty tipe =
    fromString tipe ""


{-| -}
fromString : Type e a -> String -> Field e a
fromString (Type converters) raw =
    Field
        converters
        { raw = raw
        , processed = V.fromResult (converters.fromString raw)
        , clean = True
        }


{-| -}
fromValue : Type e a -> a -> Field e a
fromValue (Type converters) value =
    Field
        converters
        { raw = converters.toString value
        , processed = V.fromResult (converters.fromValue value)
        , clean = True
        }



-- CHANGE


{-| -}
setFromString : String -> Field e a -> Field e a
setFromString raw (Field converters state) =
    Field
        converters
        { raw = raw
        , processed = V.fromResult (converters.fromString raw)
        , clean = False
        }


{-| -}
setFromValue : a -> Field e a -> Field e a
setFromValue value (Field converters state) =
    Field
        converters
        { raw = converters.toString value
        , processed = V.fromResult (converters.fromValue value)
        , clean = False
        }


{-| -}
setError : e -> Field e a -> Field e a
setError error (Field converters state) =
    Field
        converters
        { state
            | processed = V.fail error
            , clean = False
        }


{-| -}
setErrors : e -> List e -> Field e a -> Field e a
setErrors error restErrors (Field converters state) =
    Field
        converters
        { state
            | processed = V.failWithErrors error restErrors
            , clean = False
        }


{-| -}
setCustomError : e -> Field (Error e) a -> Field (Error e) a
setCustomError =
    setError << CustomError


{-| -}
setCustomErrors : e -> List e -> Field (Error e) a -> Field (Error e) a
setCustomErrors error restErrors =
    setErrors (CustomError error) (List.map CustomError restErrors)



-- QUERY


{-| -}
isEmpty : Field e a -> Bool
isEmpty (Field _ { raw }) =
    String.isEmpty raw


{-| -}
isNonEmpty : Field e a -> Bool
isNonEmpty =
    not << isEmpty


{-| -}
isBlank : Field e a -> Bool
isBlank (Field _ { raw }) =
    String.isEmpty (String.trim raw)


{-| -}
isNonBlank : Field e a -> Bool
isNonBlank =
    not << isBlank


{-| -}
isClean : Field e a -> Bool
isClean (Field _ { clean }) =
    clean


{-| -}
isDirty : Field e a -> Bool
isDirty =
    not << isClean


{-| -}
isValid : Field e a -> Bool
isValid (Field _ { processed }) =
    V.isValid processed


{-| -}
isInvalid : Field e a -> Bool
isInvalid =
    not << isValid



-- CONVERT


{-| -}
toRawString : Field e a -> String
toRawString (Field _ { raw }) =
    raw


{-| -}
toString : Field e a -> String
toString (Field tipe { processed }) =
    processed
        |> V.map tipe.toString
        |> V.withDefault ""


{-| -}
toMaybe : Field e a -> Maybe a
toMaybe =
    V.toMaybe << toValidation


{-| -}
toResult : Field e a -> Result (List e) a
toResult =
    V.toResult << toValidation


{-| -}
toValidation : Field e a -> Validation e a
toValidation (Field _ { processed }) =
    processed


{-| -}
toConverters : Field e a -> Converters e a
toConverters (Field converters _) =
    converters


{-| -}
toType : Field e a -> Type e a
toType (Field converters _) =
    Type converters


{-| -}
toState : Field e a -> State e a
toState (Field _ state) =
    state



-- APPLICATIVE


{-| -}
applyMaybe : Field e a -> Maybe (a -> b) -> Maybe b
applyMaybe field mf =
    case ( toMaybe field, mf ) of
        ( Just a, Just f ) ->
            Just (f a)

        _ ->
            Nothing


{-| -}
applyResult : Field e a -> Result (List e) (a -> b) -> Result (List e) b
applyResult field rf =
    case ( toResult field, rf ) of
        ( Ok a, Ok f ) ->
            Ok (f a)

        ( Err e1, _ ) ->
            Err e1

        ( _, Err e2 ) ->
            Err e2


{-| -}
validate2 : (a -> b -> value) -> Field x a -> Field x b -> Validation x value
validate2 f field1 field2 =
    V.map2 f (toValidation field1) (toValidation field2)


{-| -}
validate3 : (a -> b -> c -> value) -> Field x a -> Field x b -> Field x c -> Validation x value
validate3 f field1 field2 field3 =
    V.map3 f (toValidation field1) (toValidation field2) (toValidation field3)


{-| -}
validate4 : (a -> b -> c -> d -> value) -> Field x a -> Field x b -> Field x c -> Field x d -> Validation x value
validate4 f field1 field2 field3 field4 =
    V.map4 f (toValidation field1) (toValidation field2) (toValidation field3) (toValidation field4)


{-| -}
validate5 : (a -> b -> c -> d -> e -> value) -> Field x a -> Field x b -> Field x c -> Field x d -> Field x e -> Validation x value
validate5 f field1 field2 field3 field4 field5 =
    V.map5 f (toValidation field1) (toValidation field2) (toValidation field3) (toValidation field4) (toValidation field5)


{-| -}
get : Field e a -> (a -> b) -> Validation e b
get field f =
    V.map f (toValidation field)


{-| -}
and : Field e a -> Validation e (a -> b) -> Validation e b
and field =
    V.apply (toValidation field)


{-| -}
withDefault : a -> Validation e a -> a
withDefault =
    V.withDefault


{-| -}
andMaybe : Validation e a -> Maybe a
andMaybe =
    andFinally
        { onSuccess = Just
        , onFailure = always Nothing
        }


{-| -}
andResult : Validation e a -> Result (List e) a
andResult =
    andFinally
        { onSuccess = Ok
        , onFailure = Err
        }


{-| -}
andFinally :
    { onSuccess : a -> b
    , onFailure : List e -> b
    }
    -> Validation e a
    -> b
andFinally { onSuccess, onFailure } validation =
    case V.toResult validation of
        Ok value ->
            onSuccess value

        Err errors ->
            onFailure errors



-- HELPERS


{-| -}
trim : (String -> Result (Error e) a) -> String -> Result (Error e) a
trim =
    customTrim Blank


{-| -}
customTrim : e -> (String -> Result e a) -> String -> Result e a
customTrim blank f s =
    let
        t =
            String.trim s
    in
    if String.isEmpty t then
        Err blank

    else
        f t



-- ERROR


{-| -}
type Error e
    = Blank -- The empty string or a string containing only whitespace characters
      --
      -- The string in the SyntaxError and the ValidationError should always be the string that caused the error.
      -- It should NEVER be a description of the error. CustomError is used for detail.
      --
    | SyntaxError String -- A string that cannot be converted to the desired type, for e.g. "x" isn't an Int
    | ValidationError String -- A type that isn't in the subset of the type under consideration, for e.g. 1 is an Int but not an even Int
    | CustomError e -- A more specific type of validation error


{-| -}
blankError : Error e
blankError =
    Blank


{-| -}
syntaxError : String -> Error e
syntaxError =
    SyntaxError


{-| -}
validationError : String -> Error e
validationError =
    ValidationError


{-| -}
customError : e -> Error e
customError =
    CustomError


{-| -}
errorToString :
    { onBlank : String
    , onSyntaxError : String -> String
    , onValidationError : String -> String
    , onCustomError : e -> String
    }
    -> Error e
    -> String
errorToString { onBlank, onSyntaxError, onValidationError, onCustomError } error =
    case error of
        Blank ->
            onBlank

        SyntaxError s ->
            onSyntaxError s

        ValidationError s ->
            onValidationError s

        CustomError e ->
            onCustomError e


{-| -}
mapErrorType : (x -> y) -> Error x -> Error y
mapErrorType f error =
    case error of
        Blank ->
            Blank

        SyntaxError s ->
            SyntaxError s

        ValidationError s ->
            ValidationError s

        CustomError x ->
            CustomError (f x)



-- HANDLE ERRORS


{-| -}
mapTypeError : (x -> y) -> Type x a -> Type y a
mapTypeError f (Type converters) =
    Type (mapConvertersError f converters)


{-| -}
mapError : (x -> y) -> Field x a -> Field y a
mapError f (Field converters state) =
    Field
        (mapConvertersError f converters)
        { raw = state.raw
        , processed = V.mapError f state.processed
        , clean = state.clean
        }


mapConvertersError : (x -> y) -> Converters x a -> Converters y a
mapConvertersError f converters =
    { fromString = converters.fromString >> Result.mapError f
    , fromValue = converters.fromValue >> Result.mapError f
    , toString = converters.toString
    }


{-| -}
fail : e -> Field e a -> Field e a
fail error (Field converters state) =
    Field converters { state | processed = V.fail error }


{-| -}
failWithErrors : e -> List e -> Field e a -> Field e a
failWithErrors error restErrors (Field converters state) =
    Field converters { state | processed = V.failWithErrors error restErrors }


{-| -}
prependError : e -> Field e a -> Field e a
prependError newError field =
    failWithErrors newError (allErrors field) field


{-| -}
appendError : e -> Field e a -> Field e a
appendError newError field =
    case allErrors field of
        [] ->
            fail newError field

        error :: restErrors ->
            failWithErrors error (restErrors ++ [ newError ]) field


{-| -}
firstError : Field e a -> Maybe e
firstError (Field _ { processed }) =
    V.firstError processed


{-| -}
lastError : Field e a -> Maybe e
lastError (Field _ { processed }) =
    V.lastError processed


{-| -}
allErrors : Field e a -> List e
allErrors (Field _ { processed }) =
    V.allErrors processed
