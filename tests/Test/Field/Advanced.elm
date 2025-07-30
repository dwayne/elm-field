module Test.Field.Advanced exposing (suite)

import Expect
import Field.Advanced as F exposing (Error, Type)
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Field.Advanced"
        [ primitiveSuite
        , userDefinedSuite
        ]


primitiveSuite : Test
primitiveSuite =
    describe "Primitive"
        [ intSuite
        , floatSuite
        , boolSuite
        , charSuite
        , stringSuite
        ]


intSuite : Test
intSuite =
    describe "Int"
        [ describe "int" <|
            List.map
                (testStringToValue F.int)
                [ { raw = "-1", result = Ok -1 }
                , { raw = "0", result = Ok 0 }
                , { raw = "1", result = Ok 1 }
                , { raw = " 5 ", result = Ok 5 }
                , { raw = "", result = Err F.blankError }
                , { raw = "five", result = Err (F.syntaxError "five") }
                ]
        , describe "nonNegativeInt" <|
            List.map
                (testStringToValue F.nonNegativeInt)
                [ { raw = "-1", result = Err (F.validationError "-1") }
                , { raw = "0", result = Ok 0 }
                , { raw = "1", result = Ok 1 }
                , { raw = " 5 ", result = Ok 5 }
                , { raw = "", result = Err F.blankError }
                , { raw = "five", result = Err (F.syntaxError "five") }
                ]
        , describe "positiveInt" <|
            List.map
                (testStringToValue F.positiveInt)
                [ { raw = "-1", result = Err (F.validationError "-1") }
                , { raw = "0", result = Err (F.validationError "0") }
                , { raw = "1", result = Ok 1 }
                , { raw = " 5 ", result = Ok 5 }
                , { raw = "", result = Err F.blankError }
                , { raw = "five", result = Err (F.syntaxError "five") }
                ]
        , describe "nonPositiveInt" <|
            List.map
                (testStringToValue F.nonPositiveInt)
                [ { raw = "-1", result = Ok -1 }
                , { raw = "0", result = Ok 0 }
                , { raw = "1", result = Err (F.validationError "1") }
                , { raw = " 5 ", result = Err (F.validationError "5") }
                , { raw = "", result = Err F.blankError }
                , { raw = "five", result = Err (F.syntaxError "five") }
                ]
        , describe "negativeInt" <|
            List.map
                (testStringToValue F.negativeInt)
                [ { raw = "-1", result = Ok -1 }
                , { raw = "0", result = Err (F.validationError "0") }
                , { raw = "1", result = Err (F.validationError "1") }
                , { raw = " 5 ", result = Err (F.validationError "5") }
                , { raw = "", result = Err F.blankError }
                , { raw = "five", result = Err (F.syntaxError "five") }
                ]
        , describe "subsetOfInt" <|
            let
                evens =
                    F.subsetOfInt (modBy 2 >> (==) 0)
            in
            List.map
                (testStringToValue evens)
                [ { raw = "-2", result = Ok -2 }
                , { raw = "-1", result = Err (F.validationError "-1") }
                , { raw = "0", result = Ok 0 }
                , { raw = "1", result = Err (F.validationError "1") }
                , { raw = " 2 ", result = Ok 2 }
                , { raw = "", result = Err F.blankError }
                , { raw = "five", result = Err (F.syntaxError "five") }
                ]
        ]


floatSuite : Test
floatSuite =
    describe "Float"
        [ describe "float" <|
            List.map
                (testStringToValue F.float)
                [ { raw = "-0.1", result = Ok -0.1 }
                , { raw = "0", result = Ok 0 }
                , { raw = "1.1", result = Ok 1.1 }
                , { raw = " 3.14 ", result = Ok 3.14 }
                , { raw = "", result = Err F.blankError }
                , { raw = "pi", result = Err (F.syntaxError "pi") }
                ]
        , describe "nonNegativeFloat" <|
            List.map
                (testStringToValue F.nonNegativeFloat)
                [ { raw = "-0.1", result = Err (F.validationError "-0.1") }
                , { raw = "0", result = Ok 0 }
                , { raw = "1.1", result = Ok 1.1 }
                , { raw = " 3.14 ", result = Ok 3.14 }
                , { raw = "", result = Err F.blankError }
                , { raw = "pi", result = Err (F.syntaxError "pi") }
                ]
        , describe "positiveFloat" <|
            List.map
                (testStringToValue F.positiveFloat)
                [ { raw = "-0.1", result = Err (F.validationError "-0.1") }
                , { raw = "0", result = Err (F.validationError "0") }
                , { raw = "1.1", result = Ok 1.1 }
                , { raw = " 3.14 ", result = Ok 3.14 }
                , { raw = "", result = Err F.blankError }
                , { raw = "pi", result = Err (F.syntaxError "pi") }
                ]
        , describe "nonPositiveFloat" <|
            List.map
                (testStringToValue F.nonPositiveFloat)
                [ { raw = "-0.1", result = Ok -0.1 }
                , { raw = "0", result = Ok 0 }
                , { raw = "1.1", result = Err (F.validationError "1.1") }
                , { raw = " 3.14 ", result = Err (F.validationError "3.14") }
                , { raw = "", result = Err F.blankError }
                , { raw = "pi", result = Err (F.syntaxError "pi") }
                ]
        , describe "negativeFloat" <|
            List.map
                (testStringToValue F.negativeFloat)
                [ { raw = "-0.1", result = Ok -0.1 }
                , { raw = "0", result = Err (F.validationError "0") }
                , { raw = "1.1", result = Err (F.validationError "1.1") }
                , { raw = " 3.14 ", result = Err (F.validationError "3.14") }
                , { raw = "", result = Err F.blankError }
                , { raw = "pi", result = Err (F.syntaxError "pi") }
                ]
        , describe "subsetOfFloat" <|
            let
                roundsToTwo =
                    F.subsetOfFloat (round >> (==) 2)
            in
            List.map
                (testStringToValue roundsToTwo)
                [ { raw = "1.3", result = Err (F.validationError "1.3") }
                , { raw = "1.5", result = Ok 1.5 }
                , { raw = "1.8", result = Ok 1.8 }
                , { raw = "2.0", result = Ok 2.0 }
                , { raw = "2.3", result = Ok 2.3 }
                , { raw = "2.5", result = Err (F.validationError "2.5") }
                , { raw = " 3.14 ", result = Err (F.validationError "3.14") }
                , { raw = "", result = Err F.blankError }
                , { raw = "pi", result = Err (F.syntaxError "pi") }
                ]
        ]


boolSuite : Test
boolSuite =
    describe "Bool"
        [ describe "bool" <|
            List.map
                (testStringToValue F.bool)
                [ { raw = "true", result = Ok True }
                , { raw = "TrUe", result = Ok True }
                , { raw = "false", result = Ok False }
                , { raw = "fAlSe", result = Ok False }
                , { raw = " 1 ", result = Ok True }
                , { raw = " 0 ", result = Ok False }
                , { raw = "", result = Err F.blankError }
                , { raw = "not", result = Err (F.syntaxError "not") }
                ]
        , describe "true" <|
            List.map
                (testStringToValue F.true)
                [ { raw = "true", result = Ok True }
                , { raw = "TrUe", result = Ok True }
                , { raw = "false", result = Err (F.validationError "false") }
                , { raw = "fAlSe", result = Err (F.validationError "false") }
                , { raw = " 1 ", result = Ok True }
                , { raw = " 0 ", result = Err (F.validationError "false") }
                , { raw = "", result = Err F.blankError }
                , { raw = "not", result = Err (F.syntaxError "not") }
                ]
        , describe "false" <|
            List.map
                (testStringToValue F.false)
                [ { raw = "true", result = Err (F.validationError "true") }
                , { raw = "TrUe", result = Err (F.validationError "true") }
                , { raw = "false", result = Ok False }
                , { raw = "fAlSe", result = Ok False }
                , { raw = " 1 ", result = Err (F.validationError "true") }
                , { raw = " 0 ", result = Ok False }
                , { raw = "", result = Err F.blankError }
                , { raw = "not", result = Err (F.syntaxError "not") }
                ]
        , describe "customSubsetOfBool" <|
            let
                subsetOfBool : (Bool -> Bool) -> Type MyError Bool
                subsetOfBool =
                    F.customSubsetOfBool
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
            in
            [ describe "bool" <|
                List.map
                    (testStringToValue bool)
                    [ { raw = "#t", result = Ok True }
                    , { raw = "#T", result = Err (MySyntaxError "#T") }
                    , { raw = "#f", result = Ok False }
                    , { raw = "#F", result = Err (MySyntaxError "#F") }
                    , { raw = " #t ", result = Ok True }
                    , { raw = " #f ", result = Ok False }
                    , { raw = "", result = Err MyBlank }
                    , { raw = "not", result = Err (MySyntaxError "not") }
                    ]
            , describe "true" <|
                List.map
                    (testStringToValue true)
                    [ { raw = "#t", result = Ok True }
                    , { raw = "#T", result = Err (MySyntaxError "#T") }
                    , { raw = "#f", result = Err (MyValidationError "#f") }
                    , { raw = "#F", result = Err (MySyntaxError "#F") }
                    , { raw = " #t ", result = Ok True }
                    , { raw = " #f ", result = Err (MyValidationError "#f") }
                    , { raw = "", result = Err MyBlank }
                    , { raw = "not", result = Err (MySyntaxError "not") }
                    ]
            , describe "false" <|
                List.map
                    (testStringToValue false)
                    [ { raw = "#t", result = Err (MyValidationError "#t") }
                    , { raw = "#T", result = Err (MySyntaxError "#T") }
                    , { raw = "#f", result = Ok False }
                    , { raw = "#F", result = Err (MySyntaxError "#F") }
                    , { raw = " #t ", result = Err (MyValidationError "#t") }
                    , { raw = " #f ", result = Ok False }
                    , { raw = "", result = Err MyBlank }
                    , { raw = "not", result = Err (MySyntaxError "not") }
                    ]
            ]
        , describe "case insensitivity" <|
            let
                subsetOfBool : (Bool -> Bool) -> Type (Error e) Bool
                subsetOfBool =
                    F.customSubsetOfBool
                        F.defaultErrors
                        { truthy = Set.fromList [ "TrUe" ]
                        , falsy = Set.fromList [ "fAlSe" ]
                        , toString = F.defaultBoolToString
                        , caseSensitive = False
                        }

                bool : Type (Error e) Bool
                bool =
                    subsetOfBool (always True)
            in
            [ describe "bool" <|
                List.map
                    (testStringToValue bool)
                    [ { raw = "true", result = Ok True }
                    , { raw = "tRuE", result = Ok True }
                    , { raw = "TrUe", result = Ok True }
                    , { raw = " TRUE ", result = Ok True }
                    , { raw = "false", result = Ok False }
                    , { raw = "fAlSe", result = Ok False }
                    , { raw = "FaLsE", result = Ok False }
                    , { raw = " FALSE ", result = Ok False }
                    ]
            ]
        ]


type MyError
    = MyBlank
    | MySyntaxError String
    | MyValidationError String


charSuite : Test
charSuite =
    describe "Char"
        [ describe "char" <|
            List.map
                (testStringToValue F.char)
                [ { raw = "a", result = Ok 'a' }
                , { raw = "A", result = Ok 'A' }
                , { raw = "8", result = Ok '8' }
                , { raw = "\n", result = Ok '\n' }
                , { raw = "", result = Err F.blankError }
                , { raw = " ", result = Ok ' ' }
                , { raw = "  ", result = Err F.blankError }
                , { raw = "aA", result = Err (F.syntaxError "aA") }
                , { raw = " 2 ", result = Err (F.syntaxError " 2 ") }
                ]
        , describe "subsetOfChar" <|
            let
                digit =
                    F.subsetOfChar Char.isDigit
            in
            List.map
                (testStringToValue digit)
                [ { raw = "a", result = Err (F.validationError "a") }
                , { raw = "A", result = Err (F.validationError "A") }
                , { raw = "8", result = Ok '8' }
                , { raw = "\n", result = Err (F.validationError "\n") }
                , { raw = "", result = Err F.blankError }
                , { raw = " ", result = Err (F.validationError " ") }
                , { raw = "  ", result = Err F.blankError }
                , { raw = "aA", result = Err (F.syntaxError "aA") }
                , { raw = " 2 ", result = Err (F.syntaxError " 2 ") }
                ]
        ]


stringSuite : Test
stringSuite =
    describe "String"
        [ describe "string" <|
            List.map
                (testStringToValue F.string)
                [ { raw = "Hello", result = Ok "Hello" }
                , { raw = " Hello ", result = Ok "Hello" }
                , { raw = "", result = Ok "" }
                , { raw = " \n\t ", result = Ok "" }
                ]
        , describe "subsetOfString" <|
            let
                atMost3 =
                    F.subsetOfString (String.length >> (>=) 3)
            in
            List.map
                (testStringToValue atMost3)
                [ { raw = "p", result = Ok "p" }
                , { raw = "pi", result = Ok "pi" }
                , { raw = "pie", result = Ok "pie" }
                , { raw = " pie ", result = Ok "pie" }
                , { raw = "Hello", result = Err (F.validationError "Hello") }
                , { raw = " Hello ", result = Err (F.validationError "Hello") }
                , { raw = "", result = Ok "" }
                , { raw = " \n\t ", result = Ok "" }
                ]
        , describe "nonEmptyString" <|
            List.map
                (testStringToValue F.nonEmptyString)
                [ { raw = "Hello", result = Ok "Hello" }
                , { raw = " Hello ", result = Ok " Hello " }
                , { raw = "", result = Err F.blankError }
                , { raw = " \n\t ", result = Ok " \n\t " }
                ]
        , describe "subsetOfNonEmptyString" <|
            let
                hello =
                    F.subsetOfNonEmptyString ((==) "Hello")
            in
            List.map
                (testStringToValue hello)
                [ { raw = "Hello", result = Ok "Hello" }
                , { raw = " Hello ", result = Err (F.validationError " Hello ") }
                , { raw = "", result = Err F.blankError }
                , { raw = " \n\t ", result = Err (F.validationError " \n\t ") }
                ]
        , describe "nonBlankString" <|
            List.map
                (testStringToValue F.nonBlankString)
                [ { raw = "Hello", result = Ok "Hello" }
                , { raw = " Hello ", result = Ok "Hello" }
                , { raw = "", result = Err F.blankError }
                , { raw = " \n\t ", result = Err F.blankError }
                ]
        , describe "subsetOfNonBlankString" <|
            let
                atMost3 =
                    F.subsetOfNonBlankString (String.length >> (>=) 3)
            in
            List.map
                (testStringToValue atMost3)
                [ { raw = "p", result = Ok "p" }
                , { raw = "pi", result = Ok "pi" }
                , { raw = "pie", result = Ok "pie" }
                , { raw = " pie ", result = Ok "pie" }
                , { raw = "Hello", result = Err (F.validationError "Hello") }
                , { raw = " Hello ", result = Err (F.validationError "Hello") }
                , { raw = "", result = Err F.blankError }
                , { raw = " \n\t ", result = Err F.blankError }
                ]
        ]


userDefinedSuite : Test
userDefinedSuite =
    describe "User-defined"
        [ describe "subsetOfType" <|
            let
                positiveEvens =
                    F.subsetOfType (modBy 2 >> (==) 0) F.positiveInt
            in
            List.map
                (testStringToValue positiveEvens)
                [ { raw = " 2 ", result = Ok 2 }
                , { raw = "-2", result = Err (F.validationError "-2") }
                , { raw = "-1", result = Err (F.validationError "-1") }
                , { raw = "0", result = Err (F.validationError "0") }
                , { raw = "1", result = Err (F.validationError "1") }
                , { raw = "", result = Err F.blankError }
                , { raw = "five", result = Err (F.syntaxError "five") }
                ]
        , describe "customType" <|
            List.map
                (testStringToValue email)
                [ { raw = "a@b.c", result = Ok (Email "a@b.c") }
                , { raw = " a@b.c ", result = Ok (Email "a@b.c") }
                , { raw = "", result = Err F.blankError }
                , { raw = " ab.c ", result = Err (F.syntaxError "ab.c") }
                ]
        ]


type Email
    = Email String


email : Type (Error e) Email
email =
    F.customType
        { fromString =
            F.trim
                (\s ->
                    if s |> String.contains "@" then
                        Ok (Email s)

                    else
                        Err (F.syntaxError s)
                )
        , toString =
            \(Email s) -> s
        }


testStringToValue :
    Type e a
    ->
        { raw : String
        , result : Result e a
        }
    -> Test
testStringToValue tipe { raw, result } =
    test ("given \"" ++ raw ++ "\"") <|
        \_ ->
            (F.typeToConverters tipe).fromString raw
                |> Expect.equal result
