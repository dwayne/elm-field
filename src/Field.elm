module Field exposing
    ( Field, FieldString
    , Type, TypeString
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
    , toRawString, toString, toMaybe, toResult, toType
    , Validation, toValidation
    , Converters, typeToConverters, toConverters
    , validate2, validate3, validate4, validate5
    , applyMaybe, applyResult, applyValidation, succeed, validationToResult
    , trim
    , Error, blankError, syntaxError, validationError, customError, errorToString
    , mapTypeError
    , mapError
    , firstError, lastError, allErrors
    )

{-|


# Field

@docs Field, FieldString


# Type

@docs Type, TypeString


# Primitive


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

TODO: Explain about empty and blank strings.

@docs string, subsetOfString
@docs nonEmptyString, subsetOfNonEmptyString
@docs nonBlankString, subsetOfNonBlankString


# User-defined

@docs subsetOfType, customType


# Optional

@docs optional


# Construct

@docs empty, fromString, fromValue


# Change

@docs setFromString, setFromValue, setError, setErrors, setCustomError, setCustomErrors


# Query

@docs isEmpty, isNonEmpty, isBlank, isNonBlank, isClean, isDirty, isValid, isInvalid


# Convert

@docs toRawString, toString, toMaybe, toResult, toType


# Validation

@docs Validation, toValidation


# Converters

@docs Converters, typeToConverters, toConverters


# Validate

@docs validate2, validate3, validate4, validate5


# Apply

@docs applyMaybe, applyResult, applyValidation, succeed, validationToResult


# Helpers

@docs trim


# Error

@docs Error, blankError, syntaxError, validationError, customError, errorToString


# Handle Errors

@docs mapTypeError
@docs mapError
@docs firstError, lastError, allErrors

-}

import Field.Advanced as F
import Set exposing (Set)
import Validation as V



-- FIELD


{-| -}
type alias Field a =
    F.Field Error a


{-| -}
type alias FieldString =
    F.Field Never String


{-| -}
type alias Validation e a =
    V.Validation e a



-- TYPE


{-| -}
type alias Type a =
    F.Type Error a


{-| -}
type alias TypeString =
    F.Type Never String


{-| -}
type alias Converters e a =
    F.Converters e a



-- TYPE: INT


{-| -}
int : Type Int
int =
    F.int


{-| -}
nonNegativeInt : Type Int
nonNegativeInt =
    F.nonNegativeInt


{-| -}
positiveInt : Type Int
positiveInt =
    F.positiveInt


{-| -}
nonPositiveInt : Type Int
nonPositiveInt =
    F.nonPositiveInt


{-| -}
negativeInt : Type Int
negativeInt =
    F.negativeInt


{-| -}
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
string : TypeString
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



-- TYPE: CONVERT


{-| -}
typeToConverters : F.Type e a -> Converters e a
typeToConverters =
    F.typeToConverters



-- HELPERS


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
toString : F.Field e a -> String
toString =
    F.toString


{-| -}
toConverters : F.Field e a -> F.Converters e a
toConverters =
    F.toConverters


{-| -}
toType : F.Field e a -> F.Type e a
toType =
    F.toType



-- VALIDATE


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


{-| -}
succeed : F.Field e a -> (a -> b) -> Validation e b
succeed =
    F.succeed


{-| -}
validationToResult : Validation e a -> Result (List e) a
validationToResult =
    F.validationToResult



-- HANDLE ERRORS


{-| -}
mapTypeError : (x -> y) -> F.Type x a -> F.Type y a
mapTypeError =
    F.mapTypeError


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
