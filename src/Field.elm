module Field exposing
    ( Field
    , Type
    , int, nonNegativeInt, positiveInt, nonPositiveInt, negativeInt, subsetOfInt
    , float, nonNegativeFloat, positiveFloat, nonPositiveFloat, negativeFloat, subsetOfFloat
    , bool, true, false, customSubsetOfBool
    , CustomBoolOptions, defaultCustomBoolOptions, defaultTruthy, defaultFalsy, defaultBoolToString
    , char, subsetOfChar
    , string, subsetOfString
    , nonEmptyString, subsetOfNonEmptyString
    , nonBlankString, subsetOfNonBlankString
    , subsetOfType, customType
    , optional
    , empty, fromString, fromValue
    , setFromString, setFromValue, setError, setErrors, setCustomError, setCustomErrors
    , isEmpty, isNonEmpty, isBlank, isNonBlank, isClean, isDirty, isValid, isInvalid
    , toRawString, toString, toMaybe, toResult, toValidation
    , Validation, succeed, validationToResult
    , Converters, typeToConverters, toConverters
    , validate, validate2, validate3, validate4, validate5
    , applyMaybe, applyResult, applyValidation
    , trim
    , Error, blankError, syntaxError, validationError, customError, errorToString
    , mapError
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

@docs int, nonNegativeInt, positiveInt, nonPositiveInt, negativeInt, subsetOfInt


# Float

@docs float, nonNegativeFloat, positiveFloat, nonPositiveFloat, negativeFloat, subsetOfFloat


# Bool

@docs bool, true, false, customSubsetOfBool
@docs CustomBoolOptions, defaultCustomBoolOptions, defaultTruthy, defaultFalsy, defaultBoolToString


# Char

@docs char, subsetOfChar


# String

A **blank** string is either the empty string or a non-empty string consisting entirely of whitespace characters.

The empty string: `""`

Some blank strings: `""`, `" "`, `"  "`, `" \t "`, `"\n \t \r"`

@docs string, subsetOfString
@docs nonEmptyString, subsetOfNonEmptyString
@docs nonBlankString, subsetOfNonBlankString


# User-defined

@docs subsetOfType, customType


# Optional

@docs optional


# Construct

All newly constructed fields are clean.

@docs empty, fromString, fromValue


# Change

Any change to a field using these setter functions makes the field dirty. Furthermore,
these are the only functions that can dirty a field.

@docs setFromString, setFromValue, setError, setErrors, setCustomError, setCustomErrors


# Query

@docs isEmpty, isNonEmpty, isBlank, isNonBlank, isClean, isDirty, isValid, isInvalid


# Convert

@docs toRawString, toString, toMaybe, toResult, toValidation


# Validation

@docs Validation, succeed, validationToResult


# Converters

@docs Converters, typeToConverters, toConverters


# Validate

@docs validate, validate2, validate3, validate4, validate5


# Apply

[`Field e`](Field-Advanced#Field) is not applicative. However, `Maybe`, `Result e`, and `Validaton e` are all applicative.
The functions below attempt to make it convenient to do an applicative style of programming with fields by
leveraging the applicative nature of `Maybe`, `Result e`, and `Validation e`.

@docs applyMaybe, applyResult, applyValidation


# String Helpers

@docs trim


# Error

@docs Error, blankError, syntaxError, validationError, customError, errorToString


# Handle Errors

@docs mapError
@docs firstError, lastError, allErrors

-}

import Field.Advanced as F
import Set exposing (Set)
import Validation as V



-- FIELD


{-| Tracks the raw `String` input and either the value of type `a` that the `String` represents or
the errors of type [`Error`](#Error) that can accumulate.
-}
type alias Field a =
    F.Field Error a


{-| Re-export the `Validation` type for convenience.
-}
type alias Validation e a =
    V.Validation e a


{-| Useful to use with [`applyValidation`](#applyValidation).

Let `f` represent a function that takes `N >= 1` arguments. Then,

    succeed f
        |> applyValidation field1
        |> applyValidation field2
        |> ...
        |> applyValidation fieldN

applies `f` to `N` arguments as long as no field has an error.

-}
succeed : a -> Validation e a
succeed =
    F.succeed


{-| Convert from a `Validaton` to a `Result` for convenience.

Useful to use with [`applyValidation`](#applyValidation).

Let `f` represent a function that takes `N >= 1` arguments. Then,

    succeed f
        |> applyValidation field1
        |> applyValidation field2
        |> ...
        |> applyValidation fieldN
        |> validationToResult

applies `f` to `N` arguments as long as no field has an error.

-}
validationToResult : Validation e a -> Result (List e) a
validationToResult =
    F.validationToResult



-- TYPE


{-| Represents the type information for a field. It specifies how a `String` is converted to a type `a`
and back to a `String`. Conversion may result in errors of type [`Error`](#Error).
-}
type alias Type a =
    F.Type Error a


{-| The collection of conversion functions that comprise a `Type`. The conversion functions are useful
for building new field types from existing field types.

    type Positive
        = Positive Int

    fromString : String -> Result Error Positive
    fromString =
        (typeToConverters positiveInt).fromString >> Result.map Positive

    toString : Positive -> String
    toString (Positive p) =
        (typeToConverters positiveInt).toString p

    fieldType : Type Positive
    fieldType =
        customType
            { fromString = fromString
            , toString = toString
            }

-}
type alias Converters e a =
    F.Converters e a


{-| -}
typeToConverters : F.Type e a -> Converters e a
typeToConverters =
    F.typeToConverters



-- TYPE: INT


{-| Any `Int` that can be parsed from a trimmed string using `String.toInt`.

    (typeToConverters int).fromString "-1" == Ok -1

    (typeToConverters int).fromString "0" == Ok 0

    (typeToConverters int).fromString "1" == Ok 1

    (typeToConverters int).fromString " 5 " == Ok 5

    (typeToConverters int).fromString "" == Err blankError

    (typeToConverters int).fromString "five" == Err (syntaxError "five")

-}
int : Type Int
int =
    F.int


{-| Any `Int`, `n`, that can be parsed from a trimmed string using `String.toInt` such that `n >= 0`.

    (typeToConverters nonNegativeInt).fromString "-1" == Err (validationError "-1")

    (typeToConverters nonNegativeInt).fromString "0" == Ok 0

    (typeToConverters nonNegativeInt).fromString "1" == Ok 1

    (typeToConverters nonNegativeInt).fromString " 5 " == Ok 5

    (typeToConverters nonNegativeInt).fromString "" == Err blankError

    (typeToConverters nonNegativeInt).fromString "five" == Err (syntaxError "five")

-}
nonNegativeInt : Type Int
nonNegativeInt =
    F.nonNegativeInt


{-| Any `Int`, `n`, that can be parsed from a trimmed string using `String.toInt` such that `n > 0`.

    (typeToConverters positiveInt).fromString "-1" == Err (validationError "-1")

    (typeToConverters positiveInt).fromString "0" == Err (validationError "0")

    (typeToConverters positiveInt).fromString "1" == Ok 1

    (typeToConverters positiveInt).fromString " 5 " == Ok 5

    (typeToConverters positiveInt).fromString "" == Err blankError

    (typeToConverters positiveInt).fromString "five" == Err (syntaxError "five")

-}
positiveInt : Type Int
positiveInt =
    F.positiveInt


{-| Any `Int`, `n`, that can be parsed from a trimmed string using `String.toInt` such that `n <= 0`.

    (typeToConverters nonPositiveInt).fromString "-1" == Ok -1

    (typeToConverters nonPositiveInt).fromString "0" == Ok 0

    (typeToConverters nonPositiveInt).fromString "1" == Err (validationError "1")

    (typeToConverters nonPositiveInt).fromString " 5 " == Err (validationError "5")

    (typeToConverters nonPositiveInt).fromString "" == Err blankError

    (typeToConverters nonPositiveInt).fromString "five" == Err (syntaxError "five")

-}
nonPositiveInt : Type Int
nonPositiveInt =
    F.nonPositiveInt


{-| Any `Int`, `n`, that can be parsed from a trimmed string using `String.toInt` such that `n < 0`.

    (typeToConverters negativeInt).fromString "-1" == Ok -1

    (typeToConverters negativeInt).fromString "0" == Err (validationError "0")

    (typeToConverters negativeInt).fromString "1" == Err (validationError "1")

    (typeToConverters negativeInt).fromString " 5 " == Err (validationError "5")

    (typeToConverters negativeInt).fromString "" == Err blankError

    (typeToConverters negativeInt).fromString "five" == Err (syntaxError "five")

-}
negativeInt : Type Int
negativeInt =
    F.negativeInt


{-| Any `Int`, `n`, that can be parsed from a trimmed string using `String.toInt` such that `isGood n` is `True`.

    evens = subsetOfInt (modBy 2 >> (==) 0)

    (typeToConverters evens).fromString "-2" == Ok -2

    (typeToConverters evens).fromString "-1" == Err (validationError "-1")

    (typeToConverters evens).fromString "0" == Ok 0

    (typeToConverters evens).fromString "1" == Err (validationError "1")

    (typeToConverters evens).fromString " 2 " == Ok 2

    (typeToConverters evens).fromString "" == Err blankError

    (typeToConverters evens).fromString "five" == Err (syntaxError "five")

-}
subsetOfInt : (Int -> Bool) -> Type Int
subsetOfInt =
    F.subsetOfInt



-- TYPE: FLOAT


{-| -}
float : Type Float
float =
    F.float


{-| -}
nonNegativeFloat : Type Float
nonNegativeFloat =
    F.nonNegativeFloat


{-| -}
positiveFloat : Type Float
positiveFloat =
    F.positiveFloat


{-| -}
nonPositiveFloat : Type Float
nonPositiveFloat =
    F.nonPositiveFloat


{-| -}
negativeFloat : Type Float
negativeFloat =
    F.negativeFloat


{-| -}
subsetOfFloat : (Float -> Bool) -> Type Float
subsetOfFloat =
    F.subsetOfFloat



-- TYPE: BOOL


{-| -}
bool : Type Bool
bool =
    F.bool


{-| -}
true : Type Bool
true =
    F.true


{-| -}
false : Type Bool
false =
    F.false


{-| -}
type alias CustomBoolOptions =
    F.CustomBoolOptions


{-| -}
defaultCustomBoolOptions : CustomBoolOptions
defaultCustomBoolOptions =
    F.defaultCustomBoolOptions


{-| -}
defaultTruthy : Set String
defaultTruthy =
    F.defaultTruthy


{-| -}
defaultFalsy : Set String
defaultFalsy =
    F.defaultFalsy


{-| -}
defaultBoolToString : Bool -> String
defaultBoolToString =
    F.defaultBoolToString


{-| -}
customSubsetOfBool : CustomBoolOptions -> (Bool -> Bool) -> Type Bool
customSubsetOfBool =
    F.customSubsetOfBool
        { blankError = F.blankError
        , syntaxError = F.syntaxError
        , validationError = F.validationError
        }



-- TYPE: CHAR


{-| -}
char : Type Char
char =
    F.char


{-| -}
subsetOfChar : (Char -> Bool) -> Type Char
subsetOfChar =
    F.subsetOfChar



-- TYPE: STRING


{-| -}
string : F.Type e String
string =
    F.string


{-| -}
subsetOfString : (String -> Bool) -> Type String
subsetOfString =
    F.subsetOfString


{-| -}
nonEmptyString : Type String
nonEmptyString =
    F.nonEmptyString


{-| -}
subsetOfNonEmptyString : (String -> Bool) -> Type String
subsetOfNonEmptyString =
    F.subsetOfNonEmptyString


{-| -}
nonBlankString : Type String
nonBlankString =
    F.nonBlankString


{-| -}
subsetOfNonBlankString : (String -> Bool) -> Type String
subsetOfNonBlankString =
    F.subsetOfNonBlankString



-- TYPE: USER-DEFINED


{-| -}
subsetOfType : (a -> Bool) -> Type a -> Type a
subsetOfType =
    F.subsetOfType


{-| -}
customType :
    { fromString : String -> Result Error a
    , toString : a -> String
    }
    -> Type a
customType =
    F.customType



-- TYPE: OPTIONAL


{-| -}
optional : Type a -> Type (Maybe a)
optional =
    F.optional



-- CONSTRUCT


{-| -}
empty : F.Type e a -> F.Field e a
empty =
    F.empty


{-| -}
fromString : F.Type e a -> String -> F.Field e a
fromString =
    F.fromString


{-| -}
fromValue : F.Type e a -> a -> F.Field e a
fromValue =
    F.fromValue



-- CHANGE


{-| -}
setFromString : String -> F.Field e a -> F.Field e a
setFromString =
    F.setFromString


{-| -}
setFromValue : a -> F.Field e a -> F.Field e a
setFromValue =
    F.setFromValue


{-| -}
setError : Error -> Field a -> Field a
setError =
    F.setError


{-| -}
setErrors : Error -> List Error -> Field a -> Field a
setErrors =
    F.setErrors


{-| -}
setCustomError : String -> Field a -> Field a
setCustomError =
    F.setCustomError


{-| -}
setCustomErrors : String -> List String -> Field a -> Field a
setCustomErrors =
    F.setCustomErrors



-- QUERY


{-| -}
isEmpty : F.Field e a -> Bool
isEmpty =
    F.isEmpty


{-| -}
isNonEmpty : F.Field e a -> Bool
isNonEmpty =
    F.isNonEmpty


{-| -}
isBlank : F.Field e a -> Bool
isBlank =
    F.isBlank


{-| -}
isNonBlank : F.Field e a -> Bool
isNonBlank =
    F.isNonBlank


{-| -}
isClean : F.Field e a -> Bool
isClean =
    F.isClean


{-| -}
isDirty : F.Field e a -> Bool
isDirty =
    F.isDirty


{-| -}
isValid : F.Field e a -> Bool
isValid =
    F.isValid


{-| -}
isInvalid : F.Field e a -> Bool
isInvalid =
    F.isInvalid



-- CONVERT


{-| -}
toRawString : F.Field e a -> String
toRawString =
    F.toRawString


{-| -}
toString : F.Field e a -> String
toString =
    F.toString


{-| -}
toMaybe : F.Field e a -> Maybe a
toMaybe =
    F.toMaybe


{-| -}
toResult : F.Field e a -> Result (List e) a
toResult =
    F.toResult


{-| -}
toValidation : F.Field e a -> Validation e a
toValidation =
    F.toValidation


{-| -}
toConverters : F.Field e a -> F.Converters e a
toConverters =
    F.toConverters



-- VALIDATE


{-| -}
validate : (a -> value) -> F.Field x a -> Validation x value
validate =
    F.validate


{-| -}
validate2 : (a -> b -> value) -> F.Field x a -> F.Field x b -> Validation x value
validate2 =
    F.validate2


{-| -}
validate3 : (a -> b -> c -> value) -> F.Field x a -> F.Field x b -> F.Field x c -> Validation x value
validate3 =
    F.validate3


{-| -}
validate4 : (a -> b -> c -> d -> value) -> F.Field x a -> F.Field x b -> F.Field x c -> F.Field x d -> Validation x value
validate4 =
    F.validate4


{-| -}
validate5 : (a -> b -> c -> d -> e -> value) -> F.Field x a -> F.Field x b -> F.Field x c -> F.Field x d -> F.Field x e -> Validation x value
validate5 =
    F.validate5



-- APPLY


{-| -}
applyMaybe : F.Field e a -> Maybe (a -> b) -> Maybe b
applyMaybe =
    F.applyMaybe


{-| -}
applyResult : F.Field e a -> Result (List e) (a -> b) -> Result (List e) b
applyResult =
    F.applyResult


{-| -}
applyValidation : F.Field e a -> Validation e (a -> b) -> Validation e b
applyValidation =
    F.applyValidation



-- STRING HELPERS


{-| -}
trim : (String -> Result Error a) -> String -> Result Error a
trim =
    F.trim



-- ERROR


{-| -}
type alias Error =
    F.Error String


{-| -}
blankError : Error
blankError =
    F.blankError


{-| -}
syntaxError : String -> Error
syntaxError =
    F.syntaxError


{-| -}
validationError : String -> Error
validationError =
    F.validationError


{-| -}
customError : String -> Error
customError =
    F.customError


{-| -}
errorToString :
    { onBlank : String
    , onSyntaxError : String -> String
    , onValidationError : String -> String
    }
    -> Error
    -> String
errorToString { onBlank, onSyntaxError, onValidationError } =
    F.errorToString
        { onBlank = onBlank
        , onSyntaxError = onSyntaxError
        , onValidationError = onValidationError
        , onCustomError = identity
        }



-- HANDLE ERRORS


{-| -}
mapError : (x -> y) -> F.Field x a -> F.Field y a
mapError =
    F.mapError


{-| -}
firstError : Field a -> Maybe Error
firstError =
    F.firstError


{-| -}
lastError : Field a -> Maybe Error
lastError =
    F.lastError


{-| -}
allErrors : Field a -> List Error
allErrors =
    F.allErrors
