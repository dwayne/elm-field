module Field.Advanced exposing
    ( Field
    , Type
    , int, nonNegativeInt, positiveInt, nonPositiveInt, negativeInt, subsetOfInt, customSubsetOfInt
    , float, nonNegativeFloat, positiveFloat, nonPositiveFloat, negativeFloat, subsetOfFloat, customSubsetOfFloat
    , bool, true, false, customSubsetOfBool
    , CustomBoolOptions, defaultCustomBoolOptions, defaultTruthy, defaultFalsy, defaultBoolToString
    , char, subsetOfChar, customSubsetOfChar
    , string, subsetOfString, customSubsetOfString
    , nonEmptyString, subsetOfNonEmptyString, customSubsetOfNonEmptyString
    , nonBlankString, subsetOfNonBlankString, customSubsetOfNonBlankString
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
    , Error, defaultErrors, blankError, syntaxError, validationError, customError, errorToString, mapErrorType
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

A **blank** string is either the empty string or a non-empty string consisting entirely of whitespace characters.

The empty string: `""`

Some blank strings: `""`, `" "`, `"  "`, `" \t "`, `"\n \t \r"`

@docs string, subsetOfString, customSubsetOfString
@docs nonEmptyString, subsetOfNonEmptyString, customSubsetOfNonEmptyString
@docs nonBlankString, subsetOfNonBlankString, customSubsetOfNonBlankString


# User-defined

@docs subsetOfType, customSubsetOfType, customType


# Optional

@docs optional, customOptional


# Construct

All newly constructed fields are clean.

@docs empty, fromString, fromValue


# Change

Any change to a field using one of these set functions makes the field dirty.

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

@docs Error, defaultErrors, blankError, syntaxError, validationError, customError, errorToString, mapErrorType


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


{-| Tracks the raw `String` input and either the value of type `a` that the `String` represents or the errors of type `e` that can accumulate.
-}
type Field e a
    = Field (Converters e a) (State e a)


{-| The internal state of a `Field`.

`raw` contains the unprocessed `String` input.

`processed` contains the result of parsing `raw`. Either it was successfully parsed into a type `a` or there were errors of type `e`.

`clean` indicates whether or not the corresponding `Field` is considered to be **clean** or **dirty**. A `Field` is **clean** if it has
never been changed, via one of `setFromString`, `setFromValue`, `setError`, `setErrors`, `setCustomError`, or `setCustomErrors`, after
it was initially created. Otherwise, the `Field` is considered to be **dirty**.

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


{-| Represents the type information for a field. It specifies how a `String` is converted to a type `a`
and back to a `String`. Conversion may result in errors of type `e`.
-}
type Type e a
    = Type (Converters e a)


{-| The collection of conversion functions that comprise a `Type`. The conversion functions are useful
for building new field types from existing field types.

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


{-| -}
typeToConverters : Type e a -> Converters e a
typeToConverters (Type converters) =
    converters



-- TYPE: INT


{-| Any `Int` that can be parsed from a trimmed string using `String.toInt`.

    (typeToConverters int).fromString "-1" == Ok -1

    (typeToConverters int).fromString "0" == Ok 0

    (typeToConverters int).fromString "1" == Ok 1

    (typeToConverters int).fromString " 5 " == Ok 5

    (typeToConverters int).fromString "" == Err blankError

    (typeToConverters int).fromString "five" == Err (syntaxError "five")

-}
int : Type (Error e) Int
int =
    subsetOfInt (always True)


{-| Any `Int`, `n`, that can be parsed from a trimmed string using `String.toInt` such that `n >= 0`.

    (typeToConverters nonNegativeInt).fromString "-1" == Err (validationError "-1")

    (typeToConverters nonNegativeInt).fromString "0" == Ok 0

    (typeToConverters nonNegativeInt).fromString "1" == Ok 1

    (typeToConverters nonNegativeInt).fromString " 5 " == Ok 5

    (typeToConverters nonNegativeInt).fromString "" == Err blankError

    (typeToConverters nonNegativeInt).fromString "five" == Err (syntaxError "five")

-}
nonNegativeInt : Type (Error e) Int
nonNegativeInt =
    subsetOfInt ((<=) 0)


{-| Any `Int`, `n`, that can be parsed from a trimmed string using `String.toInt` such that `n > 0`.

    (typeToConverters positiveInt).fromString "-1" == Err (validationError "-1")

    (typeToConverters positiveInt).fromString "0" == Err (validationError "0")

    (typeToConverters positiveInt).fromString "1" == Ok 1

    (typeToConverters positiveInt).fromString " 5 " == Ok 5

    (typeToConverters positiveInt).fromString "" == Err blankError

    (typeToConverters positiveInt).fromString "five" == Err (syntaxError "five")

-}
positiveInt : Type (Error e) Int
positiveInt =
    subsetOfInt ((<) 0)


{-| Any `Int`, `n`, that can be parsed from a trimmed string using `String.toInt` such that `n <= 0`.

    (typeToConverters nonPositiveInt).fromString "-1" == Ok -1

    (typeToConverters nonPositiveInt).fromString "0" == Ok 0

    (typeToConverters nonPositiveInt).fromString "1" == Err (validationError "1")

    (typeToConverters nonPositiveInt).fromString " 5 " == Err (validationError "5")

    (typeToConverters nonPositiveInt).fromString "" == Err blankError

    (typeToConverters nonPositiveInt).fromString "five" == Err (syntaxError "five")

-}
nonPositiveInt : Type (Error e) Int
nonPositiveInt =
    subsetOfInt ((>=) 0)


{-| Any `Int`, `n`, that can be parsed from a trimmed string using `String.toInt` such that `n < 0`.

    (typeToConverters negativeInt).fromString "-1" == Ok -1

    (typeToConverters negativeInt).fromString "0" == Err (validationError "0")

    (typeToConverters negativeInt).fromString "1" == Err (validationError "1")

    (typeToConverters negativeInt).fromString " 5 " == Err (validationError "5")

    (typeToConverters negativeInt).fromString "" == Err blankError

    (typeToConverters negativeInt).fromString "five" == Err (syntaxError "five")

-}
negativeInt : Type (Error e) Int
negativeInt =
    subsetOfInt ((>) 0)


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
subsetOfInt : (Int -> Bool) -> Type (Error e) Int
subsetOfInt =
    customSubsetOfInt defaultErrors


{-| Similar to [`subsetOfInt`](#subsetOfInt) but you get to customize the errors.
-}
customSubsetOfInt :
    { blankError : e
    , syntaxError : String -> e
    , validationError : String -> e
    }
    -> (Int -> Bool)
    -> Type e Int
customSubsetOfInt errors isGood =
    customInt
        { blankError = errors.blankError
        , syntaxError = errors.syntaxError
        }
        (\n ->
            if isGood n then
                Ok n

            else
                Err (errors.validationError <| String.fromInt n)
        )


customInt :
    { blankError : e
    , syntaxError : String -> e
    }
    -> (Int -> Result e Int)
    -> Type e Int
customInt errors validate =
    Type
        { fromString =
            customTrim errors.blankError
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


{-| Any `Float` that can be parsed from a trimmed string using `String.toFloat`.

    (typeToConverters float).fromString "-0.1" == Ok -0.1

    (typeToConverters float).fromString "0" == Ok 0

    (typeToConverters float).fromString "1.1" == Ok 1.1

    (typeToConverters float).fromString " 3.14 " == Ok 3.14

    (typeToConverters float).fromString "" == Err blankError

    (typeToConverters float).fromString "pi" == Err (syntaxError "pi")

-}
float : Type (Error e) Float
float =
    subsetOfFloat (always True)


{-| Any `Float`, `f`, that can be parsed from a trimmed string using `String.toFloat` such that `f >= 0`.

    (typeToConverters nonNegativeFloat).fromString "-0.1" == Err (validationError "-0.1")

    (typeToConverters nonNegativeFloat).fromString "0" == Ok 0

    (typeToConverters nonNegativeFloat).fromString "1.1" == Ok 1.1

    (typeToConverters nonNegativeFloat).fromString " 3.14 " == Ok 3.14

    (typeToConverters nonNegativeFloat).fromString "" == Err blankError

    (typeToConverters nonNegativeFloat).fromString "pi" == Err (syntaxError "pi")

-}
nonNegativeFloat : Type (Error e) Float
nonNegativeFloat =
    subsetOfFloat ((<=) 0)


{-| Any `Float`, `f`, that can be parsed from a trimmed string using `String.toFloat` such that `f > 0`.

    (typeToConverters positiveFloat).fromString "-0.1" == Err (validationError "-0.1")

    (typeToConverters positiveFloat).fromString "0" == Err (validationError "0")

    (typeToConverters positiveFloat).fromString "1.1" == Ok 1.1

    (typeToConverters positiveFloat).fromString " 3.14 " == Ok 3.14

    (typeToConverters positiveFloat).fromString "" == Err blankError

    (typeToConverters positiveFloat).fromString "pi" == Err (syntaxError "pi")

-}
positiveFloat : Type (Error e) Float
positiveFloat =
    subsetOfFloat ((<) 0)


{-| Any `Float`, `f`, that can be parsed from a trimmed string using `String.toFloat` such that `f <= 0`.

    (typeToConverters nonPositiveFloat).fromString "-0.1" == Ok -0.1

    (typeToConverters nonPositiveFloat).fromString "0" == Ok 0

    (typeToConverters nonPositiveFloat).fromString "1.1" == Err (validationError "1.1")

    (typeToConverters nonPositiveFloat).fromString " 3.14 " == Err (validationError "3.14")

    (typeToConverters nonPositiveFloat).fromString "" == Err blankError

    (typeToConverters nonPositiveFloat).fromString "pi" == Err (syntaxError "pi")

-}
nonPositiveFloat : Type (Error e) Float
nonPositiveFloat =
    subsetOfFloat ((>=) 0)


{-| Any `Float`, `f`, that can be parsed from a trimmed string using `String.toFloat` such that `f < 0`.

    (typeToConverters negativeFloat).fromString "-0.1" == Ok -0.1

    (typeToConverters negativeFloat).fromString "0" == Err (validationError "0")

    (typeToConverters negativeFloat).fromString "1.1" == Err (validationError "1.1")

    (typeToConverters negativeFloat).fromString " 3.14 " == Err (validationError "3.14")

    (typeToConverters negativeFloat).fromString "" == Err blankError

    (typeToConverters negativeFloat).fromString "pi" == Err (syntaxError "pi")

-}
negativeFloat : Type (Error e) Float
negativeFloat =
    subsetOfFloat ((>) 0)


{-| Any `Float`, `f`, that can be parsed from a trimmed string using `String.toFloat` such that `isGood f` is `True`.

    roundsToTwo = subsetOfFloat (round >> (==) 2)

    (typeToConverters roundsToTwo).fromString "1.3" == Err (validationError "1.3")

    (typeToConverters roundsToTwo).fromString "1.5" == Ok 1.5

    (typeToConverters roundsToTwo).fromString "1.8" == Ok 1.8

    (typeToConverters roundsToTwo).fromString "2.0" == Ok 2.0

    (typeToConverters roundsToTwo).fromString "2.3" == Ok 2.3

    (typeToConverters roundsToTwo).fromString "2.5" == Err (validationError "2.5")

    (typeToConverters roundsToTwo).fromString "3.14" == Err (validationError "3.14")

    (typeToConverters roundsToTwo).fromString "" == Err blankError

    (typeToConverters roundsToTwo).fromString "pi" == Err (syntaxError "pi")

-}
subsetOfFloat : (Float -> Bool) -> Type (Error e) Float
subsetOfFloat =
    customSubsetOfFloat defaultErrors


{-| Similar to [`subsetOfFloat`](#subsetOfFloat) but you get to customize the errors.
-}
customSubsetOfFloat :
    { blankError : e
    , syntaxError : String -> e
    , validationError : String -> e
    }
    -> (Float -> Bool)
    -> Type e Float
customSubsetOfFloat errors isGood =
    customFloat
        { blankError = errors.blankError
        , syntaxError = errors.syntaxError
        }
        (\f ->
            if isGood f then
                Ok f

            else
                Err (errors.validationError <| String.fromFloat f)
        )


customFloat :
    { blankError : e
    , syntaxError : String -> e
    }
    -> (Float -> Result e Float)
    -> Type e Float
customFloat errors validate =
    Type
        { fromString =
            customTrim errors.blankError
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


{-| Any case-insensitive [`defaultTruthy`](#defaultTruthy) or [`defaultFalsy`](#defaultFalsy) trimmed string.

    (typeToConverters bool).fromString "true" == Ok True

    (typeToConverters bool).fromString "TrUe" == Ok True

    (typeToConverters bool).fromString "false" == Ok False

    (typeToConverters bool).fromString "fAlSe" == Ok False

    (typeToConverters bool).fromString " 1 " == Ok True

    (typeToConverters bool).fromString " 0 " == Ok False

    (typeToConverters bool).fromString "" == Err blankError

    (typeToConverters bool).fromString "not" == Err (syntaxError "not")

-}
bool : Type (Error e) Bool
bool =
    subsetOfBool (always True)


{-| Any case-insensitive [`defaultTruthy`](#defaultTruthy) trimmed string.

    (typeToConverters true).fromString "true" == Ok True

    (typeToConverters true).fromString "TrUe" == Ok True

    (typeToConverters true).fromString "false" == Err (validationError "false")

    (typeToConverters true).fromString "fAlSe" == Err (validationError "false")

    (typeToConverters true).fromString " 1 " == Ok True

    (typeToConverters true).fromString " 0 " == Err (validationError "false")

    (typeToConverters true).fromString "" == Err blankError

    (typeToConverters true).fromString "not" == Err (syntaxError "not")

-}
true : Type (Error e) Bool
true =
    subsetOfBool ((==) True)


{-| Any case-insensitive [`defaultFalsy`](#defaultFalsy) trimmed string.

    (typeToConverters false).fromString "true" == Err (validationError "true")

    (typeToConverters false).fromString "TrUe" == Err (validationError "true")

    (typeToConverters false).fromString "false" == Ok False

    (typeToConverters false).fromString "fAlSe" == Ok False

    (typeToConverters false).fromString " 1 " == Err (validationError "true")

    (typeToConverters false).fromString " 0 " == Ok False

    (typeToConverters false).fromString "" == Err blankError

    (typeToConverters false).fromString "not" == Err (syntaxError "not")

-}
false : Type (Error e) Bool
false =
    subsetOfBool ((==) False)


subsetOfBool : (Bool -> Bool) -> Type (Error e) Bool
subsetOfBool =
    customSubsetOfBool
        defaultErrors
        defaultCustomBoolOptions


{-| Customize the errors and options.

    type MyError
        = MyBlank
        | MySyntaxError String
        | MyValidationError String

    subsetOfBool : (Bool -> Bool) -> Type MyError Bool
    subsetOfBool =
        customSubsetOfBool
            { blankError = MyBlank
            , syntaxError = MySyntaxError
            , validationError = MyValidationError
            }
            { truthy = Set.fromList [ "#t" ]
            , falsy = Set.fromList [ "#f" ]
            , toString =
                \b ->
                    if b then
                        "#t"

                    else
                        "#f"
            , caseSensitive = True
            }

    bool : Type MyError Bool
    bool =
        subsetOfBool (always True)

    true : Type MyError Bool
    true =
        subsetOfBool ((==) True)

    false : Type MyError Bool
    false =
        subsetOfBool ((==) False)

-}
customSubsetOfBool :
    { blankError : e
    , syntaxError : String -> e
    , validationError : String -> e
    }
    -> CustomBoolOptions
    -> (Bool -> Bool)
    -> Type e Bool
customSubsetOfBool errors options isGood =
    customBool
        { blankError = errors.blankError
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
    { blankError : e
    , syntaxError : String -> e
    }
    -> CustomBoolOptions
    -> (Bool -> Result e Bool)
    -> Type e Bool
customBool errors options validate =
    let
        ( truthy, falsy ) =
            if options.caseSensitive then
                ( options.truthy, options.falsy )

            else
                ( Set.map String.toLower options.truthy
                , Set.map String.toLower options.falsy
                )
    in
    Type
        { fromString =
            customTrim errors.blankError
                (\s ->
                    let
                        t =
                            if options.caseSensitive then
                                s

                            else
                                String.toLower s
                    in
                    if Set.member t truthy then
                        validate True

                    else if Set.member t falsy then
                        validate False

                    else
                        Err (errors.syntaxError s)
                )
        , fromValue = validate
        , toString = options.toString
        }


{-| Used by [`customSubsetOfBool`](#customSubsetOfBool) to customize the options.
-}
type alias CustomBoolOptions =
    { truthy : Set String
    , falsy : Set String
    , toString : Bool -> String
    , caseSensitive : Bool
    }


{-| The default options used by [`bool`](#bool), [`true`](#true), and [`false`](#false).

    { truthy = defaultTruthy
    , falsy = defaultFalsy
    , toString = defaultBoolToString
    , caseSensitive = False
    }

-}
defaultCustomBoolOptions : CustomBoolOptions
defaultCustomBoolOptions =
    { truthy = defaultTruthy
    , falsy = defaultFalsy
    , toString = defaultBoolToString
    , caseSensitive = False
    }


{-| The default truthy strings: `"true"`, `"1"`, `"yes"`, `"on"`, `"y"`, and `"enabled"`.
-}
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


{-| The default falsy strings: `"false"`, `"0"`, `"no"`, `"off"`, `"n"`, and `"disabled"`.
-}
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


{-|

    defaultBoolToString True == "true"

    defaultBoolToString False == "false"

-}
defaultBoolToString : Bool -> String
defaultBoolToString b =
    if b then
        "true"

    else
        "false"



-- TYPE: CHAR


{-| Any `Char` that can be parsed from a string of exactly length one. The string is not trimmed.

    (typeToConverters char).fromString "a" == Ok 'a'

    (typeToConverters char).fromString "A" == Ok 'A'

    (typeToConverters char).fromString "8" == Ok '8'

    (typeToConverters char).fromString "\n" == Ok '\n'

    (typeToConverters char).fromString "" == Err blankError

    (typeToConverters char).fromString " " == Ok ' '

    (typeToConverters char).fromString "  " == Err blankError

    (typeToConverters char).fromString "aA" == Err (syntaxError "aA")

    (typeToConverters char).fromString " 2 " == Err (syntaxError " 2 ")

-}
char : Type (Error e) Char
char =
    subsetOfChar (always True)


{-| Any `Char`, `c`, that can be parsed from a string of exactly length one such that `isGood c` is `True`. The string is not trimmed.

    digit = subsetOfChar Char.isDigit

    (typeToConverters digit).fromString "a" == Err (validationError "a")

    (typeToConverters digit).fromString "A" == Err (validationError "A")

    (typeToConverters digit).fromString "8" == Ok '8'

    (typeToConverters digit).fromString "\n" == Err (validationError "\n")

    (typeToConverters digit).fromString "" == Err blankError

    (typeToConverters digit).fromString " " == Err (validationError " ")

    (typeToConverters digit).fromString "  " == Err blankError

    (typeToConverters digit).fromString "aA" == Err (syntaxError "aA")

    (typeToConverters digit).fromString " 2 " == Err (syntaxError " 2 ")

-}
subsetOfChar : (Char -> Bool) -> Type (Error e) Char
subsetOfChar =
    customSubsetOfChar defaultErrors


{-| Similar to [`subsetOfChar`](#subsetOfChar) but you get to customize the errors.
-}
customSubsetOfChar :
    { blankError : e
    , syntaxError : String -> e
    , validationError : String -> e
    }
    -> (Char -> Bool)
    -> Type e Char
customSubsetOfChar errors isGood =
    let
        validate ch =
            if isGood ch then
                Ok ch

            else
                Err (errors.validationError <| String.fromChar ch)
    in
    Type
        { fromString =
            \s ->
                case String.uncons s of
                    Just ( ch, "" ) ->
                        validate ch

                    Just _ ->
                        Err <|
                            if String.isEmpty (String.trim s) then
                                errors.blankError

                            else
                                errors.syntaxError s

                    Nothing ->
                        Err errors.blankError
        , fromValue = validate
        , toString = String.fromChar
        }



-- TYPE: STRING


{-| Any `String` is trimmed and accepted. It never fails.

    (typeToConverters string).fromString "Hello" == Ok "Hello"

    (typeToConverters string).fromString " Hello " == Ok "Hello"

    (typeToConverters string).fromString "" == Ok ""

    (typeToConverters string).fromString " \n\t " == Ok ""

-}
string : Type e String
string =
    customString Ok


{-| Any `String`, `s`, is trimmed and only accepted if `isGood s` is `True`.

    atMost3 = subsetOfString (String.length >> (>=) 3)

    (typeToConverters atMost3).fromString "p" == Ok "p"

    (typeToConverters atMost3).fromString "pi" == Ok "pi"

    (typeToConverters atMost3).fromString "pie" == Ok "pie"

    (typeToConverters atMost3).fromString " pie " == Ok "pie"

    (typeToConverters atMost3).fromString "Hello" == Err (validationError "Hello")

    (typeToConverters atMost3).fromString " Hello " == Err (validationError "Hello")

    (typeToConverters atMost3).fromString "" == Ok ""

    (typeToConverters atMost3).fromString " \n\t " == Ok ""

-}
subsetOfString : (String -> Bool) -> Type (Error e) String
subsetOfString =
    customSubsetOfString ValidationError


{-| Similar to [`subsetOfString`](#subsetOfString) but you get to customize the validation error.
-}
customSubsetOfString : (String -> e) -> (String -> Bool) -> Type e String
customSubsetOfString toValidationError =
    customString << customValidateStringWith toValidationError


customString : (String -> Result e String) -> Type e String
customString f =
    let
        validate =
            String.trim >> f
    in
    Type
        { fromString = validate
        , fromValue = validate
        , toString = identity
        }


{-| Any `String` except the empty string is accepted. The string is not trimmed.

    (typeToConverters nonEmptyString).fromString "Hello" == Ok "Hello"

    (typeToConverters nonEmptyString).fromString " Hello " == Ok " Hello "

    (typeToConverters nonEmptyString).fromString "" == Err blankError

    (typeToConverters nonEmptyString).fromString " \n\t " == Ok " \n\t "

-}
nonEmptyString : Type (Error e) String
nonEmptyString =
    customNonEmptyString Blank Ok


{-| Any non-empty `String`, `s`, is only accepted if `isGood s` is `True`. The string is not trimmed.

    hello = subsetOfNonEmptyString ((==) "Hello")

    (typeToConverters hello).fromString "Hello" == Ok "Hello"

    (typeToConverters hello).fromString " Hello " == Err (validationError " Hello ")

    (typeToConverters hello).fromString "" == Err blankError

    (typeToConverters hello).fromString " \n\t " == Err (validationError " \n\t ")

-}
subsetOfNonEmptyString : (String -> Bool) -> Type (Error e) String
subsetOfNonEmptyString =
    customSubsetOfNonEmptyString
        { blankError = Blank
        , validationError = ValidationError
        }


{-| Similar to [`subsetOfNonEmptyString`](#subsetOfNonEmptyString) but you get to customize the errors.
-}
customSubsetOfNonEmptyString :
    { blankError : e
    , validationError : String -> e
    }
    -> (String -> Bool)
    -> Type e String
customSubsetOfNonEmptyString errors =
    customNonEmptyString errors.blankError << customValidateStringWith errors.validationError


customNonEmptyString : e -> (String -> Result e String) -> Type e String
customNonEmptyString blank f =
    let
        validate s =
            if String.isEmpty s then
                Err blank

            else
                f s
    in
    Type
        { fromString = validate
        , fromValue = validate
        , toString = identity
        }


{-| Any non-blank `String` is trimmed and accepted.

    (typeToConverters nonBlankString).fromString "Hello" == Ok "Hello"

    (typeToConverters nonBlankString).fromString " Hello " == Ok "Hello"

    (typeToConverters nonBlankString).fromString "" == Err blankError

    (typeToConverters nonBlankString).fromString " \n\t " == Err blankError

-}
nonBlankString : Type (Error e) String
nonBlankString =
    customString (trim Ok)


{-| Any non-blank `String`, `s`, is trimmed and only accepted if `isGood s` is `True`.

    atMost3 = subsetOfNonBlankString (String.length >> (>=) 3)

    (typeToConverters atMost3).fromString "p" == Ok "p"

    (typeToConverters atMost3).fromString "pi" == Ok "pi"

    (typeToConverters atMost3).fromString "pie" == Ok "pie"

    (typeToConverters atMost3).fromString " pie " == Ok "pie"

    (typeToConverters atMost3).fromString "Hello" == Err (validationError "Hello")

    (typeToConverters atMost3).fromString " Hello " == Err (validationError "Hello")

    (typeToConverters atMost3).fromString "" == Err blankError

    (typeToConverters atMost3).fromString " \n\t " == Err blankError

-}
subsetOfNonBlankString : (String -> Bool) -> Type (Error e) String
subsetOfNonBlankString =
    customSubsetOfNonBlankString
        { blankError = Blank
        , validationError = ValidationError
        }


{-| Similar to [`subsetOfNonBlankString`](#subsetOfNonBlankString) but you get to customize the errors.
-}
customSubsetOfNonBlankString :
    { blankError : e
    , validationError : String -> e
    }
    -> (String -> Bool)
    -> Type e String
customSubsetOfNonBlankString errors =
    customNonBlankString errors.blankError << customValidateStringWith errors.validationError


customNonBlankString : e -> (String -> Result e String) -> Type e String
customNonBlankString blank f =
    let
        validate =
            customTrim blank f
    in
    Type
        { fromString = validate
        , fromValue = validate
        , toString = identity
        }


customValidateStringWith : (String -> e) -> (String -> Bool) -> String -> Result e String
customValidateStringWith toValidationError isGood s =
    if isGood s then
        Ok s

    else
        Err (toValidationError s)



-- TYPE: USER-DEFINED


{-| Accept a subset of the values accepted by the given type.

    positiveEvens = subsetOfType (modBy 2 >> (==) 0) positiveInt

    (typeToConverters positiveEvens).fromString " 2 " == Ok 2

    (typeToConverters positiveEvens).fromString "-2" == Err (validationError "-2")

    (typeToConverters positiveEvens).fromString "-1" == Err (validationError "-1")

    (typeToConverters positiveEvens).fromString "0" == Err (validationError "0")

    (typeToConverters positiveEvens).fromString "1" == Err (validationError "1")

    (typeToConverters positiveEvens).fromString "" == Err blankError

    (typeToConverters positiveEvens).fromString "five" == Err (syntaxError "five")

-}
subsetOfType : (a -> Bool) -> Type (Error e) a -> Type (Error e) a
subsetOfType =
    customSubsetOfType ValidationError


{-| Similar to [`subsetOfType`](#subsetOfType) but you get to customize the validation error.
-}
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


{-| Define any new field type.

    type Email = Email String

    email : Type (Error e) Email
    email =
        customType
            { fromString =
                trim
                    (\s ->
                        if s |> String.contains "@" then
                            Ok (Email s)
                        else
                            Err (syntaxError s)
                    )
            , toString =
                \(Email s) -> s
            }

    (typeToConverters email).fromString "a@b.c" == Ok (Email "a@b.c")

    (typeToConverters email).fromString " a@b.c " == Ok (Email "a@b.c")

    (typeToConverters email).fromString "" == Err blankError

    (typeToConverters email).fromString " ab.c " == Err (syntaxError "ab.c")

-}
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


{-| Make any field type optional.

    maybePositiveInt = optional positiveInt

    (typeToConverters maybePositiveInt).fromString "-1" == Err (validationError "-1")

    (typeToConverters maybePositiveInt).fromString "0" == Err (validationError "0")

    (typeToConverters maybePositiveInt).fromString "1" == Ok (Just 1)

    (typeToConverters maybePositiveInt).fromString " 5 " == Ok (Just 5)

    (typeToConverters maybePositiveInt).fromString "" == Ok Nothing

    (typeToConverters maybePositiveInt).fromString "five" == Err (syntaxError "five")

-}
optional : Type (Error e) a -> Type (Error e) (Maybe a)
optional =
    customOptional ((==) Blank)


{-| Similar to [`optional`](#optional) but you get to customize the blank error check.
-}
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



-- CONSTRUCT


{-| Initialize a new field from the given type and the empty string.

    empty t == fromString t ""

-}
empty : Type e a -> Field e a
empty tipe =
    fromString tipe ""


{-| Initialize a new field from the given type and string.

    toMaybe (fromString bool "enabled") == Just True

-}
fromString : Type e a -> String -> Field e a
fromString (Type converters) raw =
    Field
        converters
        { raw = raw
        , processed = V.fromResult (converters.fromString raw)
        , clean = True
        }


{-| Initialize a new field from the given type and value.

    toMaybe (fromValue char 'a') == Just 'a'

-}
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
defaultErrors :
    { blankError : Error e
    , syntaxError : String -> Error e
    , validationError : String -> Error e
    }
defaultErrors =
    { blankError = Blank
    , syntaxError = SyntaxError
    , validationError = ValidationError
    }


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
