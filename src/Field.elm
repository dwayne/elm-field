module Field exposing
    ( Error
    , Field
    , FieldBool
    , FieldString
    , Type
    , allErrors
    , and
    , andFinally
    , andMaybe
    , andResult
    , appendError
    , applyMaybe
    , applyResult
    , blankError
    , bool
    , char
    , customError
    , customFloat
    , customInt
    , customNonBlankString
    , customNonEmptyString
    , customString
    , customType
    , empty
    , errorToString
    , fail
    , failWithErrors
    , false
    , firstError
    , float
    , fromString
    , fromValue
    , get
    , int
    , isBlank
    , isClean
    , isDirty
    , isEmpty
    , isInvalid
    , isNonBlank
    , isNonEmpty
    , isValid
    , lastError
    , mapError
    , mapType
    , mapTypeError
    , nonBlankString
    , nonEmptyString
    , nonNegativeFloat
    , nonNegativeInt
    , optional
    , positiveFloat
    , positiveInt
    , prependError
    , setCustomError
    , setCustomErrors
    , setError
    , setErrors
    , setFromString
    , setFromValue
    , string
    , subsetOfChar
    , subsetOfFloat
    , subsetOfInt
    , subsetOfNonBlankString
    , subsetOfNonEmptyString
    , subsetOfString
    , subsetOfType
    , syntaxError
    , toMaybe
    , toRawString
    , toResult
    , toState
    , toString
    , toType
    , toValidation
    , trim
    , true
    , validate2
    , validate3
    , validate4
    , validate5
    , validationError
    , withDefault
    )

import Field.Advanced as F
import Validation as V exposing (Validation)



-- FIELD


type alias Field a =
    F.Field Error a


type alias FieldBool =
    F.Field Never Bool


type alias FieldString =
    F.Field Never String



-- TYPE


type alias Type a =
    F.Type Error a


int : Type Int
int =
    F.int


nonNegativeInt : Type Int
nonNegativeInt =
    F.nonNegativeInt


positiveInt : Type Int
positiveInt =
    F.positiveInt


subsetOfInt : (Int -> Bool) -> Type Int
subsetOfInt =
    F.subsetOfInt


customInt : (Int -> Result Error Int) -> Type Int
customInt =
    F.customInt
        { blank = F.blankError
        , syntaxError = F.syntaxError
        }


float : Type Float
float =
    F.float


nonNegativeFloat : Type Float
nonNegativeFloat =
    F.nonNegativeFloat


positiveFloat : Type Float
positiveFloat =
    F.positiveFloat


subsetOfFloat : (Float -> Bool) -> Type Float
subsetOfFloat =
    F.subsetOfFloat


customFloat : (Float -> Result Error Float) -> Type Float
customFloat =
    F.customFloat
        { blank = F.blankError
        , syntaxError = F.syntaxError
        }


bool : F.Type Never Bool
bool =
    F.bool


true : Type Bool
true =
    F.true


false : Type Bool
false =
    F.false


char : Type Char
char =
    F.char


subsetOfChar : (Char -> Bool) -> Type Char
subsetOfChar =
    F.subsetOfChar


optional : Type a -> Type (Maybe a)
optional =
    F.optional


string : F.Type Never String
string =
    F.string


nonEmptyString : Type String
nonEmptyString =
    F.nonEmptyString


subsetOfNonEmptyString : (String -> Bool) -> Type String
subsetOfNonEmptyString =
    F.subsetOfNonEmptyString


customNonEmptyString : (String -> Result Error String) -> Type String
customNonEmptyString =
    F.customNonEmptyString F.blankError


nonBlankString : Type String
nonBlankString =
    F.nonBlankString


subsetOfNonBlankString : (String -> Bool) -> Type String
subsetOfNonBlankString =
    F.subsetOfNonBlankString


customNonBlankString : (String -> Result Error String) -> Type String
customNonBlankString =
    F.customNonBlankString F.blankError


subsetOfString : (String -> Bool) -> Type String
subsetOfString =
    F.subsetOfString


customString : (String -> Result Error String) -> Type String
customString =
    F.customString


subsetOfType : (a -> Bool) -> Type a -> Type a
subsetOfType =
    F.subsetOfType


customType :
    { fromString : String -> Result Error a
    , toString : a -> String
    }
    -> Type a
customType =
    F.customType


trim : (String -> Result Error a) -> String -> Result Error a
trim =
    F.trim


mapType : (a -> b) -> (b -> a) -> Type a -> Type b
mapType =
    F.mapType


mapTypeError : (x -> y) -> F.Type x a -> F.Type y a
mapTypeError =
    F.mapTypeError



-- ERROR


type alias Error =
    F.Error String


blankError : Error
blankError =
    F.blankError


syntaxError : String -> Error
syntaxError =
    F.syntaxError


validationError : String -> Error
validationError =
    F.validationError


customError : String -> Error
customError =
    F.customError


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


empty : F.Type e a -> F.Field e a
empty =
    F.empty


fromString : F.Type e a -> String -> F.Field e a
fromString =
    F.fromString


fromValue : F.Type e a -> a -> F.Field e a
fromValue =
    F.fromValue



-- CHANGE


setFromString : String -> F.Field e a -> F.Field e a
setFromString =
    F.setFromString


setFromValue : a -> F.Field e a -> F.Field e a
setFromValue =
    F.setFromValue


setError : e -> F.Field e a -> F.Field e a
setError =
    fail


setErrors : e -> List e -> F.Field e a -> F.Field e a
setErrors =
    failWithErrors


setCustomError : String -> Field a -> Field a
setCustomError =
    F.setCustomError


setCustomErrors : String -> List String -> Field a -> Field a
setCustomErrors =
    F.setCustomErrors



-- QUERY


isEmpty : F.Field e a -> Bool
isEmpty =
    F.isEmpty


isNonEmpty : F.Field e a -> Bool
isNonEmpty =
    F.isNonEmpty


isBlank : F.Field e a -> Bool
isBlank =
    F.isBlank


isNonBlank : F.Field e a -> Bool
isNonBlank =
    F.isNonBlank


isClean : F.Field e a -> Bool
isClean =
    F.isClean


isDirty : F.Field e a -> Bool
isDirty =
    F.isDirty


isValid : F.Field e a -> Bool
isValid =
    F.isValid


isInvalid : F.Field e a -> Bool
isInvalid =
    F.isInvalid



-- CONVERT


toRawString : F.Field e a -> String
toRawString =
    F.toRawString


toMaybe : F.Field e a -> Maybe a
toMaybe =
    F.toMaybe


toResult : F.Field e a -> Result (List e) a
toResult =
    F.toResult


toValidation : F.Field e a -> Validation e a
toValidation =
    F.toValidation


toString : F.Field e a -> String
toString =
    F.toString


toType : F.Field e a -> F.Type e a
toType =
    F.toType


toState : F.Field e a -> F.State e a
toState =
    F.toState



-- APPLICATIVE


applyMaybe : F.Field e a -> Maybe (a -> b) -> Maybe b
applyMaybe =
    F.applyMaybe


applyResult : F.Field e a -> Result (List e) (a -> b) -> Result (List e) b
applyResult =
    F.applyResult


validate2 : (a -> b -> value) -> F.Field x a -> F.Field x b -> Validation x value
validate2 =
    F.validate2


validate3 : (a -> b -> c -> value) -> F.Field x a -> F.Field x b -> F.Field x c -> Validation x value
validate3 =
    F.validate3


validate4 : (a -> b -> c -> d -> value) -> F.Field x a -> F.Field x b -> F.Field x c -> F.Field x d -> Validation x value
validate4 =
    F.validate4


validate5 : (a -> b -> c -> d -> e -> value) -> F.Field x a -> F.Field x b -> F.Field x c -> F.Field x d -> F.Field x e -> Validation x value
validate5 =
    F.validate5


get : F.Field e a -> (a -> b) -> Validation e b
get =
    F.get


and : F.Field e a -> Validation e (a -> b) -> Validation e b
and =
    F.and


withDefault : a -> Validation e a -> a
withDefault =
    F.withDefault


andMaybe : Validation e a -> Maybe a
andMaybe =
    F.andMaybe


andResult : Validation e a -> Result (List e) a
andResult =
    F.andResult


andFinally :
    { onSuccess : a -> b
    , onFailure : List e -> b
    }
    -> Validation e a
    -> b
andFinally =
    F.andFinally



-- HANDLE ERRORS


mapError : (x -> y) -> F.Field x a -> F.Field y a
mapError =
    F.mapError


fail : e -> F.Field e a -> F.Field e a
fail =
    F.fail


failWithErrors : e -> List e -> F.Field e a -> F.Field e a
failWithErrors =
    F.failWithErrors


prependError : Error -> Field a -> Field a
prependError =
    F.prependError


appendError : Error -> Field a -> Field a
appendError =
    F.appendError


firstError : Field a -> Maybe Error
firstError =
    F.firstError


lastError : Field a -> Maybe Error
lastError =
    F.lastError


allErrors : Field a -> List Error
allErrors =
    F.allErrors
