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
type alias Converters a =
    F.Converters Error a


{-| -}
typeToConverters : Type a -> Converters a
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


{-| Any `Float` that can be parsed from a trimmed string using `String.toFloat`.

    (typeToConverters float).fromString "-0.1" == Ok -0.1

    (typeToConverters float).fromString "0" == Ok 0

    (typeToConverters float).fromString "1.1" == Ok 1.1

    (typeToConverters float).fromString " 3.14 " == Ok 3.14

    (typeToConverters float).fromString "" == Err blankError

    (typeToConverters float).fromString "pi" == Err (syntaxError "pi")

-}
float : Type Float
float =
    F.float


{-| Any `Float`, `f`, that can be parsed from a trimmed string using `String.toFloat` such that `f >= 0`.

    (typeToConverters nonNegativeFloat).fromString "-0.1" == Err (validationError "-0.1")

    (typeToConverters nonNegativeFloat).fromString "0" == Ok 0

    (typeToConverters nonNegativeFloat).fromString "1.1" == Ok 1.1

    (typeToConverters nonNegativeFloat).fromString " 3.14 " == Ok 3.14

    (typeToConverters nonNegativeFloat).fromString "" == Err blankError

    (typeToConverters nonNegativeFloat).fromString "pi" == Err (syntaxError "pi")

-}
nonNegativeFloat : Type Float
nonNegativeFloat =
    F.nonNegativeFloat


{-| Any `Float`, `f`, that can be parsed from a trimmed string using `String.toFloat` such that `f > 0`.

    (typeToConverters positiveFloat).fromString "-0.1" == Err (validationError "-0.1")

    (typeToConverters positiveFloat).fromString "0" == Err (validationError "0")

    (typeToConverters positiveFloat).fromString "1.1" == Ok 1.1

    (typeToConverters positiveFloat).fromString " 3.14 " == Ok 3.14

    (typeToConverters positiveFloat).fromString "" == Err blankError

    (typeToConverters positiveFloat).fromString "pi" == Err (syntaxError "pi")

-}
positiveFloat : Type Float
positiveFloat =
    F.positiveFloat


{-| Any `Float`, `f`, that can be parsed from a trimmed string using `String.toFloat` such that `f <= 0`.

    (typeToConverters nonPositiveFloat).fromString "-0.1" == Ok -0.1

    (typeToConverters nonPositiveFloat).fromString "0" == Ok 0

    (typeToConverters nonPositiveFloat).fromString "1.1" == Err (validationError "1.1")

    (typeToConverters nonPositiveFloat).fromString " 3.14 " == Err (validationError "3.14")

    (typeToConverters nonPositiveFloat).fromString "" == Err blankError

    (typeToConverters nonPositiveFloat).fromString "pi" == Err (syntaxError "pi")

-}
nonPositiveFloat : Type Float
nonPositiveFloat =
    F.nonPositiveFloat


{-| Any `Float`, `f`, that can be parsed from a trimmed string using `String.toFloat` such that `f < 0`.

    (typeToConverters negativeFloat).fromString "-0.1" == Ok -0.1

    (typeToConverters negativeFloat).fromString "0" == Err (validationError "0")

    (typeToConverters negativeFloat).fromString "1.1" == Err (validationError "1.1")

    (typeToConverters negativeFloat).fromString " 3.14 " == Err (validationError "3.14")

    (typeToConverters negativeFloat).fromString "" == Err blankError

    (typeToConverters negativeFloat).fromString "pi" == Err (syntaxError "pi")

-}
negativeFloat : Type Float
negativeFloat =
    F.negativeFloat


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
subsetOfFloat : (Float -> Bool) -> Type Float
subsetOfFloat =
    F.subsetOfFloat



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
bool : Type Bool
bool =
    F.bool


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
true : Type Bool
true =
    F.true


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
false : Type Bool
false =
    F.false


{-| Customize the options.

    import Set

    mySubsetOfBool : (Bool -> Bool) -> Type Bool
    mySubsetOfBool =
        customSubsetOfBool
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

    myBool : Type Bool
    myBool =
        mySubsetOfBool (always True)

    myTrue : Type Bool
    myTrue =
        mySubsetOfBool ((==) True)

    myFalse : Type Bool
    myFalse =
        mySubsetOfBool ((==) False)

-}
customSubsetOfBool : CustomBoolOptions -> (Bool -> Bool) -> Type Bool
customSubsetOfBool =
    F.customSubsetOfBool
        { blankError = F.blankError
        , syntaxError = F.syntaxError
        , validationError = F.validationError
        }


{-| Used by [`customSubsetOfBool`](#customSubsetOfBool) to customize the options.
-}
type alias CustomBoolOptions =
    F.CustomBoolOptions


{-| The default options used by [`bool`](#bool), [`true`](#true), and [`false`](#false).

    { truthy = defaultTruthy
    , falsy = defaultFalsy
    , toString = defaultBoolToString
    , caseSensitive = False
    }

-}
defaultCustomBoolOptions : CustomBoolOptions
defaultCustomBoolOptions =
    F.defaultCustomBoolOptions


{-| The default truthy strings: `"true"`, `"1"`, `"yes"`, `"on"`, `"y"`, and `"enabled"`.
-}
defaultTruthy : Set String
defaultTruthy =
    F.defaultTruthy


{-| The default falsy strings: `"false"`, `"0"`, `"no"`, `"off"`, `"n"`, and `"disabled"`.
-}
defaultFalsy : Set String
defaultFalsy =
    F.defaultFalsy


{-|

    defaultBoolToString True == "true"

    defaultBoolToString False == "false"

-}
defaultBoolToString : Bool -> String
defaultBoolToString =
    F.defaultBoolToString



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
char : Type Char
char =
    F.char


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
subsetOfChar : (Char -> Bool) -> Type Char
subsetOfChar =
    F.subsetOfChar



-- TYPE: STRING


{-| Any trimmed `String` is accepted. It never fails.

    (typeToConverters string).fromString "Hello" == Ok "Hello"

    (typeToConverters string).fromString " Hello " == Ok "Hello"

    (typeToConverters string).fromString "" == Ok ""

    (typeToConverters string).fromString " \n\t " == Ok ""

-}
string : F.Type e String
string =
    F.string


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
subsetOfString : (String -> Bool) -> Type String
subsetOfString =
    F.subsetOfString


{-| Any `String` except the empty string is accepted. The string is not trimmed.

    (typeToConverters nonEmptyString).fromString "Hello" == Ok "Hello"

    (typeToConverters nonEmptyString).fromString " Hello " == Ok " Hello "

    (typeToConverters nonEmptyString).fromString "" == Err blankError

    (typeToConverters nonEmptyString).fromString " \n\t " == Ok " \n\t "

-}
nonEmptyString : Type String
nonEmptyString =
    F.nonEmptyString


{-| Any non-empty `String`, `s`, is only accepted if `isGood s` is `True`. The string is not trimmed.

    hello = subsetOfNonEmptyString ((==) "Hello")

    (typeToConverters hello).fromString "Hello" == Ok "Hello"

    (typeToConverters hello).fromString " Hello " == Err (validationError " Hello ")

    (typeToConverters hello).fromString "" == Err blankError

    (typeToConverters hello).fromString " \n\t " == Err (validationError " \n\t ")

-}
subsetOfNonEmptyString : (String -> Bool) -> Type String
subsetOfNonEmptyString =
    F.subsetOfNonEmptyString


{-| Any non-blank trimmed `String` is accepted.

    (typeToConverters nonBlankString).fromString "Hello" == Ok "Hello"

    (typeToConverters nonBlankString).fromString " Hello " == Ok "Hello"

    (typeToConverters nonBlankString).fromString "" == Err blankError

    (typeToConverters nonBlankString).fromString " \n\t " == Err blankError

-}
nonBlankString : Type String
nonBlankString =
    F.nonBlankString


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
subsetOfNonBlankString : (String -> Bool) -> Type String
subsetOfNonBlankString =
    F.subsetOfNonBlankString



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
subsetOfType : (a -> Bool) -> Type a -> Type a
subsetOfType =
    F.subsetOfType


{-| Define a new field type.

    type Email = Email String

    email : Type Email
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
    { fromString : String -> Result Error a
    , toString : a -> String
    }
    -> Type a
customType =
    F.customType



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
optional : Type a -> Type (Maybe a)
optional =
    F.optional



-- CONSTRUCT


{-| Initialize a new field from the given type and the empty string.

For all field types, `t`, `empty t` is equivalent to `fromString t ""`.

-}
empty : Type a -> Field a
empty =
    F.empty


{-| Initialize a new field from the given type and string.

    toMaybe (fromString bool "enabled") == Just True

-}
fromString : Type a -> String -> Field a
fromString =
    F.fromString


{-| Initialize a new field from the given type and value.

    toMaybe (fromValue char 'a') == Just 'a'

-}
fromValue : Type a -> a -> Field a
fromValue =
    F.fromValue



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
setFromString : String -> Field a -> Field a
setFromString =
    F.setFromString


{-|

    field = empty (optional positiveInt)

    toResult field == Ok Nothing

    toResult (setFromValue (Just -1) field) == Err [ validationError "-1" ]

    toResult (setFromValue (Just 0) field) == Err [ validationError "0" ]

    toResult (setFromValue (Just 1) field) == Ok (Just 1)

    toResult (setFromValue (Just 5) field) == Ok (Just 5)

    toResult (setFromValue Nothing field) == Ok Nothing

-}
setFromValue : a -> Field a -> Field a
setFromValue =
    F.setFromValue


{-|

    field = empty int

    error = customError "Not an integer"

    toResult (setError error field) == Err [ error ]

-}
setError : Error -> Field a -> Field a
setError =
    F.setError


{-| -}
setErrors : Error -> List Error -> Field a -> Field a
setErrors =
    F.setErrors


{-|

    field = empty int

    error = customError "Not an integer"

    toResult (setCustomError "Not an integer" field) == Err [ error ]

-}
setCustomError : String -> Field a -> Field a
setCustomError =
    F.setCustomError


{-| -}
setCustomErrors : String -> List String -> Field a -> Field a
setCustomErrors =
    F.setCustomErrors



-- QUERY


{-| Determine whether or not the raw string is equal to the empty string.

    isEmpty (empty int) == True

    isEmpty (fromString int "   ") == False

    isEmpty (fromString int "1") == False

-}
isEmpty : Field a -> Bool
isEmpty =
    F.isEmpty


{-| `isNonEmpty f` is equivalent to `not (isEmpty f)`.

    isNonEmpty (empty int) == False

    isNonEmpty (fromString int "   ") == True

    isNonEmpty (fromString int "1") == True

-}
isNonEmpty : Field a -> Bool
isNonEmpty =
    F.isNonEmpty


{-| Determine whether or not the raw string is equal to a blank string.

    isBlank (empty int) == True

    isBlank (fromString int "   ") == True

    isBlank (fromString int "1") == False

-}
isBlank : Field a -> Bool
isBlank =
    F.isBlank


{-| `isNonBlank f` is equivalent to `not (isBlank f)`.

    isNonBlank (empty int) == False

    isNonBlank (fromString int "   ") == False

    isNonBlank (fromString int "1") == True

-}
isNonBlank : Field a -> Bool
isNonBlank =
    F.isNonBlank


{-| `True` if the field is [clean](#state).

    isClean (empty int) == True

    isClean (fromString int "1") == True

    isClean (setFromString "1" (empty int)) == False

-}
isClean : Field a -> Bool
isClean =
    F.isClean


{-| `True` if the field is [dirty](#state). `isDirty f` is equivalent to `not (isClean f)`.

    isDirty (empty int) == False

    isDirty (fromString int "1") == False

    isDirty (setFromString "1" (empty int)) == True

-}
isDirty : Field a -> Bool
isDirty =
    F.isDirty


{-| `True` if the field does not have any errors.

    isValid (empty int) == False

    isValid (fromString int "1") == True

-}
isValid : Field a -> Bool
isValid =
    F.isValid


{-| `True` if the field has errors. `isInvalid f` is equivalent to `not (isValid f)`.

    isInvalid (empty int) == True

    isInvalid (fromString int "1") == False

-}
isInvalid : Field a -> Bool
isInvalid =
    F.isInvalid



-- CONVERT


{-|

    toRawString (empty int) == ""

    toRawString (fromString int " \n5  \t \n") == " \n5  \t \n"

-}
toRawString : Field a -> String
toRawString =
    F.toRawString


{-|

    toString (empty int) == ""

    toString (fromString int " \n5  \t \n") == "5"

-}
toString : Field a -> String
toString =
    F.toString


{-|

    toMaybe (empty int) == Nothing

    toMaybe (fromString int "5") == Just 5

-}
toMaybe : Field a -> Maybe a
toMaybe =
    F.toMaybe


{-|

    toResult (empty int) == Err [ blankError ]

    toResult (fromString int "5") == Ok 5

-}
toResult : Field a -> Result (List Error) a
toResult =
    F.toResult


{-|

    import Validation as V

    toValidation (empty int) == V.fail blankError

    toValidation (fromString int "5") == V.succeed 5

-}
toValidation : Field a -> Validation Error a
toValidation =
    F.toValidation


{-| -}
toConverters : Field a -> Converters a
toConverters =
    F.toConverters



-- VALIDATE


{-| Apply a function to the processed value of a field. If the field is valid,
the value will be converted. If the field is invalid, the same error will be
propagated.

    import Validation as V

    validate sqrt (fromString float "4.0") == V.succeed 2.0

    validate sqrt (fromString float "") == V.fail blankError

-}
validate : (a -> value) -> F.Field x a -> Validation x value
validate =
    F.validate


{-| Apply a function if both fields are valid. If not, the errors will be accumulated
in order of failure.

    import Validation as V

    validate2 max (fromString int "42") (fromString int "13") == V.succeed 42

    validate2 max (fromString int "x") (fromString int "13") == V.fail (syntaxError "x")

    validate2 max (fromString int "42") (fromString int "y") == V.fail (syntaxError "y")

    validate2 max (fromString int "x") (fromString int "y") == V.failWithErrors (syntaxError "x") [ syntaxError "y" ]

-}
validate2 : (a -> b -> value) -> F.Field x a -> F.Field x b -> Validation x value
validate2 =
    F.validate2


{-| Apply a function if all three fields are valid. If not, the errors will be accumulated
in order of failure.

    validate3 (\a b c -> a + b + c) (fromString int "1") (fromString int "2") (fromString int "3") == V.succeed 6

    validate3 (\a b c -> a + b + c) (fromString int "1") (fromString int "y") (fromString int "3") == V.fail (syntaxError "y")

    validate3 (\a b c -> a + b + c) (fromString int "x") (fromString int "2") (fromString int "z") == V.failWithErrors (syntaxError "x") [ syntaxError "z" ]

    validate3 (\a b c -> a + b + c) (fromString int "x") (fromString int "y") (fromString int "z") == V.failWithErrors (syntaxError "x") [ syntaxError "y", syntaxError "z" ]

-}
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
applyMaybe : Field a -> Maybe (a -> b) -> Maybe b
applyMaybe =
    F.applyMaybe


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
applyResult : F.Field e a -> Result (List e) (a -> b) -> Result (List e) b
applyResult =
    F.applyResult


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
