module Test.Field.Advanced exposing (suite)

import Expect
import Field.Advanced as F exposing (Error, Type)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Field.Advanced"
        [ primitiveSuite
        ]


primitiveSuite : Test
primitiveSuite =
    describe "Primitive"
        [ intSuite
        , floatSuite
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


testStringToValue :
    Type (Error e) a
    ->
        { raw : String
        , result : Result (Error e) a
        }
    -> Test
testStringToValue tipe { raw, result } =
    test ("given \"" ++ raw ++ "\"") <|
        \_ ->
            (F.typeToConverters tipe).fromString raw
                |> Expect.equal result
