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
    , toRawString, toString, toMaybe, toResult, toValidation, toType
    , State, toState
    , Validation, succeed, validationToResult
    , Converters, typeToConverters, toConverters
    , validate, validate2, validate3, validate4, validate5
    , applyMaybe, applyResult, applyValidation
    , trim, customTrim
    , Error, defaultErrors, blankError, syntaxError, validationError, customError, errorToString, mapErrorType
    , mapTypeError
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

Any change to a field using these setter functions makes the field dirty. Furthermore,
these are the only functions that can dirty a field.

@docs setFromString, setFromValue, setError, setErrors, setCustomError, setCustomErrors


# Query

@docs isEmpty, isNonEmpty, isBlank, isNonBlank, isClean, isDirty, isValid, isInvalid


# Convert

@docs toRawString, toString, toMaybe, toResult, toValidation, toType


# State

@docs State, toState


# Validation

@docs Validation, succeed, validationToResult


# Converters

@docs Converters, typeToConverters, toConverters


# Validate

@docs validate, validate2, validate3, validate4, validate5


# Apply

[`Field e`](#Field) is not applicative. However, `Maybe`, `Result e`, and `Validaton e` are all applicative.
The functions below attempt to make it convenient to do an applicative style of programming with fields by
leveraging the applicative nature of `Maybe`, `Result e`, and `Validation e`.

@docs applyMaybe, applyResult, applyValidation


# String Helpers

@docs trim, customTrim


# Error

@docs Error, defaultErrors, blankError, syntaxError, validationError, customError, errorToString, mapErrorType


# Handle Errors

@docs mapTypeError
@docs mapError
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

`clean` indicates whether or not the corresponding `Field` is considered to be **clean** or **dirty**.

A `Field` is **clean** if it has never been changed, via one of `setFromString`, `setFromValue`, `setError`, `setErrors`, `setCustomError`,
or `setCustomErrors`, after it was initially created. Otherwise, the `Field` is considered to be **dirty**.

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
    V.succeed


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
    V.toResult



-- TYPE


{-| Represents the type information for a field. It specifies how a `String` is converted to a type `a`
and back to a `String`. Conversion may result in errors of type `e`.
-}
type Type e a
    = Type (Converters e a)


{-| The collection of conversion functions that comprise a `Type`. The conversion functions are useful
for building new field types from existing field types.

    type Positive
        = Positive Int

    fromString : String -> Result (Error e) Positive
    fromString =
        (typeToConverters positiveInt).fromString >> Result.map Positive

    toString : Positive -> String
    toString (Positive p) =
        (typeToConverters positiveInt).toString p

    fieldType : Type (Error e) Positive
    fieldType =
        customType
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
customInt errors makeFromValue =
    Type
        { fromString =
            customTrim errors.blankError
                (\s ->
                    case String.toInt s of
                        Just n ->
                            makeFromValue n

                        Nothing ->
                            Err (errors.syntaxError s)
                )
        , fromValue = makeFromValue
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
customFloat errors makeFromValue =
    Type
        { fromString =
            customTrim errors.blankError
                (\s ->
                    case String.toFloat s of
                        Just f ->
                            makeFromValue f

                        Nothing ->
                            Err (errors.syntaxError s)
                )
        , fromValue = makeFromValue
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

    import Set

    type MyError
        = MyBlank
        | MySyntaxError String
        | MyValidationError String

    mySubsetOfBool : (Bool -> Bool) -> Type MyError Bool
    mySubsetOfBool =
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

    myBool : Type MyError Bool
    myBool =
        mySubsetOfBool (always True)

    myTrue : Type MyError Bool
    myTrue =
        mySubsetOfBool ((==) True)

    myFalse : Type MyError Bool
    myFalse =
        mySubsetOfBool ((==) False)

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
customBool errors options makeFromValue =
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
                        makeFromValue True

                    else if Set.member t falsy then
                        makeFromValue False

                    else
                        Err (errors.syntaxError s)
                )
        , fromValue = makeFromValue
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
        makeFromValue ch =
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
                        makeFromValue ch

                    Just _ ->
                        Err <|
                            if String.isEmpty (String.trim s) then
                                errors.blankError

                            else
                                errors.syntaxError s

                    Nothing ->
                        Err errors.blankError
        , fromValue = makeFromValue
        , toString = String.fromChar
        }



-- TYPE: STRING


{-| Any trimmed `String` is accepted. It never fails.

    (typeToConverters string).fromString "Hello" == Ok "Hello"

    (typeToConverters string).fromString " Hello " == Ok "Hello"

    (typeToConverters string).fromString "" == Ok ""

    (typeToConverters string).fromString " \n\t " == Ok ""

-}
string : Type e String
string =
    customString Ok


{-| Any trimmed `String`, `s`, is accepted if `isGood s` is `True`.

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
        makeFromValue =
            String.trim >> f
    in
    Type
        { fromString = makeFromValue
        , fromValue = makeFromValue
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
        makeFromValue s =
            if String.isEmpty s then
                Err blank

            else
                f s
    in
    Type
        { fromString = makeFromValue
        , fromValue = makeFromValue
        , toString = identity
        }


{-| Any non-blank trimmed `String` is accepted.

    (typeToConverters nonBlankString).fromString "Hello" == Ok "Hello"

    (typeToConverters nonBlankString).fromString " Hello " == Ok "Hello"

    (typeToConverters nonBlankString).fromString "" == Err blankError

    (typeToConverters nonBlankString).fromString " \n\t " == Err blankError

-}
nonBlankString : Type (Error e) String
nonBlankString =
    customString (trim Ok)


{-| Any non-blank trimmed `String`, `s`, is accepted if `isGood s` is `True`.

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
        makeFromValue =
            customTrim blank f
    in
    Type
        { fromString = makeFromValue
        , fromValue = makeFromValue
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
        makeFromValue x =
            if isGood x then
                Ok x

            else
                Err (toValidationError <| converters.toString x)
    in
    Type
        { fromString = converters.fromString >> Result.andThen makeFromValue
        , fromValue = converters.fromValue >> Result.andThen makeFromValue
        , toString = converters.toString
        }


{-| Define a new field type.

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


{-|

    field = empty positiveInt

    toResult field == Err [ blankError ]

    toResult (setFromString "-1" field) == Err [ validationError "-1" ]

    toResult (setFromString "0" field) == Err [ validationError "0" ]

    toResult (setFromString "1" field) == Ok 1

    toResult (setFromString " 5 " field) == Ok 5

    toResult (setFromString "   " field) == Err [ blankError ]

    toResult (setFromString "five" field) == Err [ syntaxError "five" ]

-}
setFromString : String -> Field e a -> Field e a
setFromString raw (Field converters state) =
    Field
        converters
        { raw = raw
        , processed = V.fromResult (converters.fromString raw)
        , clean = False
        }


{-|

    field = empty (optional positiveInt)

    toResult field == Ok Nothing

    toResult (setFromValue (Just -1) field) == Err [ validationError "-1" ]

    toResult (setFromValue (Just 0) field) == Err [ validationError "0" ]

    toResult (setFromValue (Just 1) field) == Ok (Just 1)

    toResult (setFromValue (Just 5) field) == Ok (Just 5)

    toResult (setFromValue Nothing field) == Ok Nothing

-}
setFromValue : a -> Field e a -> Field e a
setFromValue value (Field converters state) =
    Field
        converters
        { raw = converters.toString value
        , processed = V.fromResult (converters.fromValue value)
        , clean = False
        }


{-|

    field = empty int

    error = customError "Not an integer"

    toResult (setError error field) == Err [ error ]

-}
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


{-|

    field = empty int

    error = customError "Not an integer"

    toResult (setCustomError "Not an integer" field) == Err [ error ]

-}
setCustomError : e -> Field (Error e) a -> Field (Error e) a
setCustomError =
    setError << CustomError


{-| -}
setCustomErrors : e -> List e -> Field (Error e) a -> Field (Error e) a
setCustomErrors error restErrors =
    setErrors (CustomError error) (List.map CustomError restErrors)



-- QUERY


{-| Determine whether or not the raw string is equal to the empty string.

    isEmpty (empty int) == True

    isEmpty (fromString int "   ") == False

    isEmpty (fromString int "1") == False

-}
isEmpty : Field e a -> Bool
isEmpty (Field _ { raw }) =
    String.isEmpty raw


{-| `isNonEmpty f` is equivalent to `not (isEmpty f)`.

    isNonEmpty (empty int) == False

    isNonEmpty (fromString int "   ") == True

    isNonEmpty (fromString int "1") == True

-}
isNonEmpty : Field e a -> Bool
isNonEmpty =
    not << isEmpty


{-| Determine whether or not the raw string is equal to a blank string.

    isBlank (empty int) == True

    isBlank (fromString int "   ") == True

    isBlank (fromString int "1") == False

-}
isBlank : Field e a -> Bool
isBlank (Field _ { raw }) =
    String.isEmpty (String.trim raw)


{-| `isNonBlank f` is equivalent to `not (isBlank f)`.

    isNonBlank (empty int) == False

    isNonBlank (fromString int "   ") == False

    isNonBlank (fromString int "1") == True

-}
isNonBlank : Field e a -> Bool
isNonBlank =
    not << isBlank


{-| `True` if the field is [clean](#state).

    isClean (empty int) == True

    isClean (fromString int "1") == True

    isClean (setFromString "1" (empty int)) == False

-}
isClean : Field e a -> Bool
isClean (Field _ { clean }) =
    clean


{-| `True` if the field is [dirty](#state). `isDirty f` is equivalent to `not (isClean f)`.

    isDirty (empty int) == False

    isDirty (fromString int "1") == False

    isDirty (setFromString "1" (empty int)) == True

-}
isDirty : Field e a -> Bool
isDirty =
    not << isClean


{-| `True` if the field does not have any errors.

    isValid (empty int) == False

    isValid (fromString int "1") == True

-}
isValid : Field e a -> Bool
isValid (Field _ { processed }) =
    V.isValid processed


{-| `True` if the field has errors. `isInvalid f` is equivalent to `not (isValid f)`.

    isInvalid (empty int) == True

    isInvalid (fromString int "1") == False

-}
isInvalid : Field e a -> Bool
isInvalid =
    not << isValid



-- CONVERT


{-|

    toRawString (empty int) == ""

    toRawString (fromString int " \n5  \t \n") == " \n5  \t \n"

-}
toRawString : Field e a -> String
toRawString (Field _ { raw }) =
    raw


{-|

    toString (empty int) == ""

    toString (fromString int " \n5  \t \n") == "5"

-}
toString : Field e a -> String
toString (Field tipe { processed }) =
    processed
        |> V.map tipe.toString
        |> V.withDefault ""


{-|

    toMaybe (empty int) == Nothing

    toMaybe (fromString int "5") == Just 5

-}
toMaybe : Field e a -> Maybe a
toMaybe =
    V.toMaybe << toValidation


{-|

    toResult (empty int) == Err [ blankError ]

    toResult (fromString int "5") == Ok 5

-}
toResult : Field e a -> Result (List e) a
toResult =
    V.toResult << toValidation


{-|

    import Validation as V

    toValidation (empty int) == V.fail blankError

    toValidation (fromString int "5") == V.succeed 5

-}
toValidation : Field e a -> Validation e a
toValidation (Field _ { processed }) =
    processed


{-| -}
toConverters : Field e a -> Converters e a
toConverters (Field converters _) =
    converters


{-| Recover the type information for a field.
-}
toType : Field e a -> Type e a
toType (Field converters _) =
    Type converters


{-| -}
toState : Field e a -> State e a
toState (Field _ state) =
    state



-- VALIDATE


{-| Apply a function to the processed value of a field. If the field is valid,
the value will be converted. If the field is invalid, the same error will be
propagated.

    import Validation as V

    validate sqrt (fromString float "4.0") == V.succeed 2.0

    validate sqrt (fromString float "") == V.fail blankError

-}
validate : (a -> value) -> Field x a -> Validation x value
validate f field =
    V.map f (toValidation field)


{-| Apply a function if both fields are valid. If not, the errors will be accumulated
in order of failure.

    import Validation as V

    validate2 max (fromString int "42") (fromString int "13") == V.succeed 42

    validate2 max (fromString int "x") (fromString int "13") == V.fail (syntaxError "x")

    validate2 max (fromString int "42") (fromString int "y") == V.fail (syntaxError "y")

    validate2 max (fromString int "x") (fromString int "y") == V.failWithErrors (syntaxError "x") [ syntaxError "y" ]

-}
validate2 : (a -> b -> value) -> Field x a -> Field x b -> Validation x value
validate2 f field1 field2 =
    V.map2 f (toValidation field1) (toValidation field2)


{-| Apply a function if all three fields are valid. If not, the errors will be accumulated
in order of failure.

    validate3 (\a b c -> a + b + c) (fromString int "1") (fromString int "2") (fromString int "3") == V.succeed 6

    validate3 (\a b c -> a + b + c) (fromString int "1") (fromString int "y") (fromString int "3") == V.fail (syntaxError "y")

    validate3 (\a b c -> a + b + c) (fromString int "x") (fromString int "2") (fromString int "z") == V.failWithErrors (syntaxError "x") [ syntaxError "z" ]

    validate3 (\a b c -> a + b + c) (fromString int "x") (fromString int "y") (fromString int "z") == V.failWithErrors (syntaxError "x") [ syntaxError "y", syntaxError "z" ]

-}
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



-- APPLY


{-| Suppose you want to use the values from multiple fields but you don't care about any of the
errors that are present.

    type Primitives
        = Positive Int Float Bool Char String
        | NonPositive Int Float Bool Char String

    let
        maybePrimitives =
            (\n f b c s ->
                if n > 0 then
                    Positive n f b c s
                else
                    NonPositive n f b c s
            )
            |> Just
            |> applyMaybe (fromString int "5")
            |> applyMaybe (fromString float "3.14")
            |> applyMaybe (fromValue bool True)
            |> applyMaybe (fromValue char 'a')
            |> applyMaybe (fromString string "Hello")
    in
    maybePrimitives == Just (Positive 5 3.14 True 'a' "Hello")

-}
applyMaybe : Field e a -> Maybe (a -> b) -> Maybe b
applyMaybe field mf =
    case ( toMaybe field, mf ) of
        ( Just a, Just f ) ->
            Just (f a)

        _ ->
            Nothing


{-| Suppose you want to use the values from multiple fields and you only care about the
first error that occurs.

    type Primitives
        = Positive Int Float Bool Char String
        | NonPositive Int Float Bool Char String

    let
        resultPrimitives =
            (\n f b c s ->
                if n > 0 then
                    Positive n f b c s
                else
                    NonPositive n f b c s
            )
            |> Ok
            |> applyResult (fromString int "5")
            |> applyResult (fromString float "pi")
            |> applyResult (fromString bool "not")
            |> applyResult (fromValue char 'a')
            |> applyResult (fromString string "Hello")
    in
    resultPrimitives == Err [ syntaxError "pi" ]

-}
applyResult : Field e a -> Result (List e) (a -> b) -> Result (List e) b
applyResult field rf =
    case ( toResult field, rf ) of
        ( Ok a, Ok f ) ->
            Ok (f a)

        ( _, Err e2 ) ->
            --
            -- Propagate the first error we find
            --
            Err e2

        ( Err e1, _ ) ->
            Err e1


{-| Suppose you want to use the values from multiple fields and you care about all the
errors that occur.

    type Primitives
        = Positive Int Float Bool Char String
        | NonPositive Int Float Bool Char String

    let
        resultPrimitives =
            (\n f b c s ->
                if n > 0 then
                    Positive n f b c s
                else
                    NonPositive n f b c s
            )
            |> succeed
            |> applyValidation (fromString int "5")
            |> applyValidation (fromString float "pi")
            |> applyValidation (fromString bool "not")
            |> applyValidation (fromValue char 'a')
            |> applyValidation (fromString string "Hello")
            |> validationToResult
    in
    resultPrimitives == Err [ syntaxError "pi", syntaxError "not" ]

-}
applyValidation : Field e a -> Validation e (a -> b) -> Validation e b
applyValidation field =
    V.apply (toValidation field)



-- STRING HELPERS


{-| Get rid of whitespace on both sides of a string.

If the trimmed string is empty return [`blankError`](#blankError),
otherwise apply the given function to the trimmed string.

It is useful when you're defining a custom field type.

    type Username = Username String

    username : Type (Error String) Username
    username =
        customType
            { fromString =
                trim
                    (\s ->
                        let
                            len =
                                String.length s
                        in
                        if len < 3 then
                            Err <| customError "The username must have at least 3 characters."
                        else if len > 25 then
                            Err <| customError "The username must have at most 25 characters."
                        else
                            Ok (Username s)
                    )
            , toString =
                \(Username s) -> s
            }


    toResult (fromString username "abc") == Ok (Username "abc")

    toResult (fromString username "ab") == Err [ customError "The username must have at least 3 characters." ]

    toResult (fromString username "abcdefghijklmnopqrstuvwxyz") == Err [ customError "The username must have at most 25 characters." ]

-}
trim : (String -> Result (Error e) a) -> String -> Result (Error e) a
trim =
    customTrim Blank


{-| Similar to [`trim`](#trim) but you get to customize the blank error.
-}
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


{-| The error type returned by the built-in field types. It supports blank, syntax, validation, and custom validation errors.
-}
type Error e
    = Blank
    | SyntaxError String
    | ValidationError String
    | CustomError e


{-| The default [blank](#blankError), [syntax](#syntaxError), and [validation](#validationError) errors. It's particularly useful when
customizing the `Bool` type with [`customSubsetOfBool`](#customSubsetOfBool) and you don't want to customize the errors.

    myBool = customSubsetOfBool defaultErrors { defaultCustomBoolOptions | caseSensitive = True } (always True)

    toResult (fromString myBool "true") == Ok True

    toResult (fromString bool "True") == Ok True

    toResult (fromString myBool "True") == Err [ syntaxError "True" ]

-}
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


{-| Indicates that the user's input string is either the empty string or a string containing only whitespace characters.

    toResult (fromString positiveInt "") == Err [ blankError ]

    toResult (fromString positiveInt "\n\t ") == Err [ blankError ]

-}
blankError : Error e
blankError =
    Blank


{-| Indicates that the user's input string cannot be converted to the desired type.

    toResult (fromString positiveInt "five") == Err [ syntaxError "five" ]

The string used in the syntax error is the trimmed user's input string that caused the error.

    toResult (fromString positiveInt " five\n \t") == Err [ syntaxError "five" ]

-}
syntaxError : String -> Error e
syntaxError =
    SyntaxError


{-| Indicates that the user's input string was successfully converted to a value of the desired type but
that the value wasn't contained in the subset of values under consideration.

    toResult (fromString positiveInt "0") == Err [ validationError "0" ]

    toResult (fromString positiveInt "-1") == Err [ validationError "-1" ]

Let `v` represent the value to which the user's input string was successfully converted. The string used
in the validation error is the string returned by the `toString` function of the field's type when it's
applied to `v`.

    v : Int
    v = -1

    field : Field (Error e) Int
    field = fromString positiveInt " -1 \n \t"

    negativeOne : String
    negativeOne = (toConverters field).toString v

    toResult field == Err [ validationError negativeOne ]

In the above example, the user's input string, `" -1 \n \t"`, would be successfully converted to the
value, `-1`, but `-1` is not a positive integer. Hence, the string representation of `-1` as determined
by `(toConverters field).toString` is used in the validation error.

-}
validationError : String -> Error e
validationError =
    ValidationError


{-| Indicates that the user's input string was successfully converted to a value of the desired type but
that the value wasn't contained in the subset of values under consideration.

It's similar to a [`validationError`](#validationError) but rather than a `String` you get to customize
the type used to record any extra error details.

    type Username = Username String

    type UsernameError
        = TooShort { actual : Int, min : Int }
        | TooLong { actual : Int, max : Int }

    username : Type (Error UsernameError) Username
    username =
        customType
            { fromString =
                trim
                    (\s ->
                        let
                            len =
                                String.length s
                        in
                        if len < 3 then
                            Err <| customError (TooShort { actual = len, min = 3 })
                        else if len > 25 then
                            Err <| customError (TooLong { actual = len, max = 25 })
                        else
                            Ok (Username s)
                    )
            , toString =
                \(Username s) -> s
            }

    toResult (fromString username "abc") == Ok (Username "abc")

    toResult (fromString username "ab") == Err [ customError (TooShort { actual = 2, min = 3 }) ]

    toResult (fromString username "abcdefghijklmnopqrstuvwxyz") == Err [ customError (TooLong { actual = 26, max = 25 }) ]

You get to design the type so that you'd have enough information available to display
a user-friendly error message. See [`errorToString`](#errorToString) for an example.

-}
customError : e -> Error e
customError =
    CustomError


{-| Convert your errors to user-friendly error messages.

Let's continue with the example from [`customError`](#customError).

    toErrorMessage : Error UsernameError -> String
    toErrorMessage =
        errorToString
            { onBlank = "The username is required."
            , onSyntaxError = always ""
            , onValidationError = always ""
            , onCustomError =
                \e ->
                    case e of
                        TooShort { actual, min } ->
                            "The username must have at least " ++ String.fromInt min ++ " characters: " ++ String.fromInt actual ++ "."
                        TooLong { actual, max } ->
                            "The username must have at most " ++ String.fromInt max ++ " characters: " ++ String.fromInt actual ++ "."
            }

    toErrorMessage (customError <| TooShort { actual = 2, min = 3 }) == "The username must have at least 3 characters: 2."

    toErrorMessage (customError <| TooLong { actual = 26, max = 25 }) == "The username must have at most 25 characters: 26."

-}
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


{-| Apply a function to the error value of the field type. It's useful for unifying
multiple custom field errors under a single error type.

Let's continue with the example from [`customError`](#customError) and incorporate an `Age` type.

    type Age = Age Int

    type AgeError
        = TooYoung { actual : Int, min : Int }
        | TooOld { actual : Int, max : Int }

    age : Type (Error AgeError) Age
    age =
        customType
            { fromString =
                (typeToConverters positiveInt).fromString
                    >> Result.andThen
                        (\n ->
                            if n < 21 then
                                Err <| customError (TooYoung { actual = n, min = 21 })
                            else if n > 65 then
                                Err <| customError (TooOld { actual = n, max = 65 })
                            else
                                Ok (Age n)
                        )
            , toString =
                \(Age n) -> String.fromInt n
            }

    --
    -- 1. The single error type under which we will unify the two custom field errors.
    --
    type Errors
        = MyUsernameError (Error UsernameError)
        | MyAgeError (Error AgeError)

    usernameField : Field (Error UsernameError) Username
    usernameField = fromString username "ab"

    ageField : Field (Error AgeError) Age
    ageField = fromString age "67"

    type alias UserDetails = { username : Username, age : Age }

    --
    -- 2. We use mapError in order to validate both fields at the same time.
    --
    resultUserDetails =
        validationToResult <|
            validate2
                UserDetails
                (usernameField |> mapError MyUsernameError)
                (ageField |> mapError MyAgeError)

    resultUserDetails == Err [ MyUsernameError (customError <| TooShort { actual = 2, min = 3 }), MyAgeError (customError <| TooOld { actual = 67, max = 65 }) ]

-}
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


{-|

    fromString string "A string"
        |> firstError
        |> (==) Nothing

    fromString string "A string"
        |> setErrors "First error" [ "Second error", "Third error" ]
        |> firstError
        |> (==) (Just "First error")

-}
firstError : Field e a -> Maybe e
firstError (Field _ { processed }) =
    V.firstError processed


{-|

    fromString string "A string"
        |> lastError
        |> (==) Nothing

    fromString string "A string"
        |> setErrors "First error" [ "Second error", "Third error" ]
        |> lastError
        |> (==) (Just "Third error")

-}
lastError : Field e a -> Maybe e
lastError (Field _ { processed }) =
    V.lastError processed


{-|

    fromString string "A string"
        |> allErrors
        |> (==) []

    fromString string "A string"
        |> setErrors "First error" [ "Second error", "Third error" ]
        |> allErrors
        |> (==) [ "First error", "Second error", "Third error" ]

-}
allErrors : Field e a -> List e
allErrors (Field _ { processed }) =
    V.allErrors processed
