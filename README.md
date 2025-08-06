# elm-field

Construct valid data from unreliable string inputs.

## Examples

### Example 1

You can create fields for all of Elm's primitive types.

```elm
import Field as F exposing (Field)


n : Field Int
n = F.fromString F.int "5"


f : Field Float
f = F.fromString F.float "3.14"


b : Field Bool
b = F.fromString F.bool "true"


c : Field Char
c = F.fromString F.char "a"


s : Field String
s = F.fromString F.string "Hello, world!"


type alias Record =
    { n : Int
    , f : Float
    , b : Bool
    , c : Char
    , s : String
    }


record : Maybe Record
record =
    Just Record
        |> F.applyMaybe n
        |> F.applyMaybe f
        |> F.applyMaybe b
        |> F.applyMaybe c
        |> F.applyMaybe s
--
-- record == Just { n = 5, f = 3.14, b = True, c = 'a', s = "Hello, world!" }
--
```

### Example 2

Suppose you need to get the age of a user from an input field. The input field gives you the user's input as a `String` which you need to interpret as the user's age. Furthermore, the user's age must be between 21 years and 35 years inclusive.

Here's the quick and dirty way to do it.

```elm
import Field as F exposing (Field, Type)


youngAdult : Type Int
youngAdult = F.subsetOfInt (\n -> n >= 21 && n <= 35)


goodAge : Field Int
goodAge = F.fromString youngAdult "23"
--
-- F.toResult goodAge == Ok 23
--


tooYoung : Field Int
tooYoung = F.fromString youngAdult "17"
--
-- F.toResult tooYoung == Err [ F.validationError "17" ]
--


tooOld : Field Int
tooOld = F.fromString youngAdult "40"
--
-- F.toResult tooOld == Err [ F.validationError "40" ]
--
```

A better approach would be to create an `Age` type with custom validation errors.

```elm
module Age exposing
    ( Age
    , CustomError(..)
    , Error
    , errorToString
    , fieldType
    , fromString
    , toString
    )

import Field.Advanced as F


type Age
    = Age Int


type alias Error =
    F.Error CustomError


type CustomError
    = TooYoung { actual : Int, min : Int }
    | TooOld { actual : Int, max : Int }


fromString : String -> Result Error Age
fromString =
    (F.typeToConverters F.int).fromString
        >> Result.andThen
            (\n ->
                if n < 21 then
                    Err (F.customError <| TooYoung { actual = n, min = 21 })

                else if n > 35 then
                    Err (F.customError <| TooOld { actual = n, max = 35 })

                else
                    Ok (Age n)
            )


toString : Age -> String
toString (Age n) =
    String.fromInt n


fieldType : F.Type Error Age
fieldType =
    F.customType
        { fromString = fromString
        , toString = toString
        }


errorToString : Error -> String
errorToString =
    F.errorToString
        { onBlank = "An age is required"
        , onSyntaxError = \s -> "The age must be an integer: " ++ s
        , onValidationError = always ""
        , onCustomError =
            \e ->
                case e of
                    TooYoung { actual, min } ->
                        "You must be at least " ++ String.fromInt min ++ " years old: " ++ String.fromInt actual

                    TooOld { actual, max } ->
                        "You must be at most " ++ String.fromInt max ++ " years old: " ++ String.fromInt actual
        }
```

And then, you could use it as follows:

```elm
import Age exposing (Age, Error)
import Field.Advanced as F exposing (Field)


goodAge : Field Error Age
goodAge = F.fromString Age.fieldType "23"
--
-- F.toResult goodAge == Ok (Age 23)
--


tooYoung : Field Error Age
tooYoung = F.fromString Age.fieldType "17"
--
-- F.toResult tooYoung == Err [ F.customError (Age.TooYoung { actual = 17, min = 21 }) ]
--


tooOld : Field Error Age
tooOld = F.fromString Age.fieldType "40"
--
-- F.toResult tooOld == Err [ F.customError (Age.TooOld { actual = 40, max = 35 }) ]
--
```

The custom errors could be converted to user-friendly error messages, using `Age.errorToString`, at a later time.

You can find the source code for many more examples in the [`examples/`](/examples) directory. And, you can fiddle with them at [https://dwayne.github.io/elm-field/](https://dwayne.github.io/elm-field/).
