module Field.Advanced exposing
    ( Error
    , Field
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
    , mapErrorType
    , nonBlankString
    , nonEmptyString
    , nonNegativeFloat
    , nonNegativeInt
    , optional
    , positiveFloat
    , positiveInt
    , prependError
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
    , toString
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

import Validation as V exposing (Validation)



-- FIELD


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


int : Type (Error e) Int
int =
    customInt Ok


nonNegativeInt : Type (Error e) Int
nonNegativeInt =
    subsetOfInt ((<=) 0)


positiveInt : Type (Error e) Int
positiveInt =
    subsetOfInt ((<) 0)


subsetOfInt : (Int -> Bool) -> Type (Error e) Int
subsetOfInt isGood =
    customInt
        (\n ->
            if isGood n then
                Ok n

            else
                Err (ValidationError (String.fromInt n))
        )


customInt : (Int -> Result (Error e) Int) -> Type (Error e) Int
customInt validate =
    { fromString =
        trim
            (\s ->
                case String.toInt s of
                    Just n ->
                        validate n

                    Nothing ->
                        Err (SyntaxError s)
            )
    , fromValue = validate
    , toString = String.fromInt
    }


float : Type (Error e) Float
float =
    customFloat Ok


nonNegativeFloat : Type (Error e) Float
nonNegativeFloat =
    subsetOfFloat ((<=) 0)


positiveFloat : Type (Error e) Float
positiveFloat =
    subsetOfFloat ((<) 0)


subsetOfFloat : (Float -> Bool) -> Type (Error e) Float
subsetOfFloat isGood =
    customFloat
        (\f ->
            if isGood f then
                Ok f

            else
                Err (ValidationError (String.fromFloat f))
        )


customFloat : (Float -> Result (Error e) Float) -> Type (Error e) Float
customFloat validate =
    { fromString =
        trim
            (\s ->
                case String.toFloat s of
                    Just f ->
                        validate f

                    Nothing ->
                        Err (SyntaxError s)
            )
    , fromValue = validate
    , toString = String.fromFloat
    }


bool : Type (Error e) Bool
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


true : Type (Error e) Bool
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


false : Type (Error e) Bool
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


char : Type (Error e) Char
char =
    subsetOfChar (always True)


subsetOfChar : (Char -> Bool) -> Type (Error e) Char
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
                    Err Blank
    , fromValue = validate
    , toString = String.fromChar
    }


optional : Type (Error e) a -> Type (Error e) (Maybe a)
optional tipe =
    --
    -- I don't think it would be wise to use optional with string fields.
    --
    { fromString =
        \s ->
            case tipe.fromString s of
                Ok value ->
                    Ok (Just value)

                Err Blank ->
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

                        Err Blank ->
                            Ok Nothing

                        Err err ->
                            Err err

                Nothing ->
                    Ok Nothing
    , toString = Maybe.map tipe.toString >> Maybe.withDefault ""
    }


string : Type (Error e) String
string =
    customString (String.trim >> Ok)



--
-- Empty string -> ""
-- Blank string -> "", " ", "  ", " \t ", "\n \t \r"
--
-- Blank includes the empty string as well as strings consisting entirely of whitespace characters.
--


nonEmptyString : Type (Error e) String
nonEmptyString =
    customNonEmptyString Ok


subsetOfNonEmptyString : (String -> Bool) -> Type (Error e) String
subsetOfNonEmptyString =
    customNonEmptyString << validateStringWith


customNonEmptyString : (String -> Result (Error e) String) -> Type (Error e) String
customNonEmptyString validate =
    customString
        (\s ->
            if String.isEmpty s then
                Err Blank

            else
                validate (String.trim s)
        )


nonBlankString : Type (Error e) String
nonBlankString =
    customString (trim Ok)


subsetOfNonBlankString : (String -> Bool) -> Type (Error e) String
subsetOfNonBlankString =
    customNonBlankString << validateStringWith


customNonBlankString : (String -> Result (Error e) String) -> Type (Error e) String
customNonBlankString validate =
    customString (trim validate)


subsetOfString : (String -> Bool) -> Type (Error e) String
subsetOfString =
    customString << validateStringWith


validateStringWith : (String -> Bool) -> String -> Result (Error e) String
validateStringWith isGood s =
    if isGood s then
        Ok s

    else
        Err (ValidationError s)


customString : (String -> Result (Error e) String) -> Type (Error e) String
customString validate =
    { fromString = validate
    , fromValue = validate
    , toString = identity
    }


subsetOfType : (a -> Bool) -> Type (Error e) a -> Type (Error e) a
subsetOfType isGood tipe =
    let
        validate value =
            if isGood value then
                Ok value

            else
                Err (ValidationError (tipe.toString value))
    in
    { fromString = tipe.fromString >> Result.andThen validate
    , fromValue = tipe.fromValue >> Result.andThen validate
    , toString = tipe.toString
    }


customType :
    { fromString : String -> Result (Error e) a
    , toString : a -> String
    }
    -> Type (Error e) a
customType options =
    { fromString = options.fromString
    , fromValue = Ok
    , toString = options.toString
    }


trim : (String -> Result (Error e) a) -> String -> Result (Error e) a
trim f s =
    let
        t =
            String.trim s
    in
    if String.isEmpty t then
        Err Blank

    else
        f t



-- CONSTRUCT


empty : Type e a -> Field e a
empty tipe =
    fromString tipe ""


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


setError : e -> Field (Error e) a -> Field (Error e) a
setError =
    fail << CustomError


setErrors : e -> List e -> Field (Error e) a -> Field (Error e) a
setErrors error errors =
    failWithErrors (CustomError error) (List.map CustomError errors)



-- QUERY


isEmpty : Field e a -> Bool
isEmpty (Field _ { raw }) =
    String.isEmpty (rawToString raw)


isNonEmpty : Field e a -> Bool
isNonEmpty =
    not << isEmpty


isBlank : Field e a -> Bool
isBlank (Field _ { raw }) =
    String.isEmpty (String.trim (rawToString raw))


isNonBlank : Field e a -> Bool
isNonBlank =
    not << isBlank


isClean : Field e a -> Bool
isClean (Field _ { raw }) =
    case raw of
        Initial _ ->
            True

        Dirty _ ->
            False


isDirty : Field e a -> Bool
isDirty =
    not << isClean


isValid : Field e a -> Bool
isValid (Field _ { processed }) =
    V.isValid processed


isInvalid : Field e a -> Bool
isInvalid =
    not << isValid



-- CONVERT


toRawString : Field e a -> String
toRawString (Field _ { raw }) =
    rawToString raw


rawToString : Raw -> String
rawToString raw =
    case raw of
        Initial s ->
            s

        Dirty s ->
            s


toMaybe : Field e a -> Maybe a
toMaybe =
    V.toMaybe << toValidation


toResult : Field e a -> Result (List e) a
toResult =
    V.toResult << toValidation


toValidation : Field e a -> Validation e a
toValidation (Field _ { processed }) =
    processed


toString : Field e a -> String
toString (Field tipe { processed }) =
    processed
        |> V.map tipe.toString
        |> V.withDefault ""



-- APPLICATIVE


applyMaybe : Field e a -> Maybe (a -> b) -> Maybe b
applyMaybe field mf =
    case ( toMaybe field, mf ) of
        ( Just a, Just f ) ->
            Just (f a)

        _ ->
            Nothing


applyResult : Field e a -> Result (List e) (a -> b) -> Result (List e) b
applyResult field rf =
    case ( toResult field, rf ) of
        ( Ok a, Ok f ) ->
            Ok (f a)

        ( Err e1, _ ) ->
            Err e1

        ( _, Err e2 ) ->
            Err e2


validate2 : (a -> b -> value) -> Field x a -> Field x b -> Validation x value
validate2 f field1 field2 =
    V.map2 f (toValidation field1) (toValidation field2)


validate3 : (a -> b -> c -> value) -> Field x a -> Field x b -> Field x c -> Validation x value
validate3 f field1 field2 field3 =
    V.map3 f (toValidation field1) (toValidation field2) (toValidation field3)


validate4 : (a -> b -> c -> d -> value) -> Field x a -> Field x b -> Field x c -> Field x d -> Validation x value
validate4 f field1 field2 field3 field4 =
    V.map4 f (toValidation field1) (toValidation field2) (toValidation field3) (toValidation field4)


validate5 : (a -> b -> c -> d -> e -> value) -> Field x a -> Field x b -> Field x c -> Field x d -> Field x e -> Validation x value
validate5 f field1 field2 field3 field4 field5 =
    V.map5 f (toValidation field1) (toValidation field2) (toValidation field3) (toValidation field4) (toValidation field5)


get : Field e a -> (a -> b) -> Validation e b
get field f =
    V.map f (toValidation field)


and : Field e a -> Validation e (a -> b) -> Validation e b
and field =
    V.apply (toValidation field)


withDefault : a -> Validation e a -> a
withDefault =
    V.withDefault


andMaybe : Validation e a -> Maybe a
andMaybe =
    andFinally
        { onSuccess = Just
        , onFailure = always Nothing
        }


andResult : Validation e a -> Result (List e) a
andResult =
    andFinally
        { onSuccess = Ok
        , onFailure = Err
        }


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



-- ERROR


type Error e
    = Blank
    | SyntaxError String
    | ValidationError String
    | CustomError e


blankError : Error e
blankError =
    Blank


syntaxError : String -> Error e
syntaxError =
    SyntaxError


validationError : String -> Error e
validationError =
    ValidationError


customError : e -> Error e
customError =
    CustomError


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


fail : e -> Field e a -> Field e a
fail error (Field tipe state) =
    Field tipe { state | processed = V.fail error }


failWithErrors : e -> List e -> Field e a -> Field e a
failWithErrors error restErrors (Field tipe state) =
    Field tipe { state | processed = V.failWithErrors error restErrors }


prependError : e -> Field e a -> Field e a
prependError newError field =
    failWithErrors newError (allErrors field) field


appendError : e -> Field e a -> Field e a
appendError newError field =
    case allErrors field of
        [] ->
            fail newError field

        error :: restErrors ->
            failWithErrors error (restErrors ++ [ newError ]) field


firstError : Field e a -> Maybe e
firstError (Field _ { processed }) =
    V.firstError processed


lastError : Field e a -> Maybe e
lastError (Field _ { processed }) =
    V.lastError processed


allErrors : Field e a -> List e
allErrors (Field _ { processed }) =
    V.allErrors processed
