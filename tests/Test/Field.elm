module Test.Field exposing (suite)

import Expect
import Field as F
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, test)
import Test.Lib.Fuzz as Fuzz


suite : Test
suite =
    describe "Field"
        [ primitiveTypesSuite
        ]


primitiveTypesSuite : Test
primitiveTypesSuite =
    describe "Primitive Types"
        [ intSuite
        , floatSuite
        ]


intSuite : Test
intSuite =
    describe "Int"
        [ describe "int"
            [ fuzz Fuzz.int "integer strings in the range Random.minInt to Random.maxInt inclusive" <|
                \n ->
                    F.fromString F.int (String.fromInt n)
                        |> F.toMaybe
                        |> Expect.equal (Just n)
            , test "integer string surrounded by whitespace" <|
                \_ ->
                    F.fromString F.int "  5 \t "
                        |> F.toMaybe
                        |> Expect.equal (Just 5)
            , fuzz Fuzz.int "integers in the range Random.minInt to Random.maxInt inclusive" <|
                \n ->
                    F.fromValue F.int n
                        |> F.toMaybe
                        |> Expect.equal (Just n)
            , fuzz Fuzz.blankString "blank strings" <|
                \s ->
                    F.fromString F.int s
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , test "non-integer string" <|
                \_ ->
                    F.fromString F.int "one"
                        |> F.toResult
                        |> Expect.equal (Err [ F.syntaxError "one" ])
            ]
        , describe "nonNegativeInt"
            [ test "zero" <|
                \_ ->
                    F.fromValue F.nonNegativeInt 0
                        |> F.toMaybe
                        |> Expect.equal (Just 0)
            , test "positive integer" <|
                \_ ->
                    F.fromValue F.nonNegativeInt 1
                        |> F.toMaybe
                        |> Expect.equal (Just 1)
            , test "negative integer" <|
                \_ ->
                    F.fromValue F.nonNegativeInt -1
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "-1" ])
            ]
        , describe "positiveInt"
            [ test "zero" <|
                \_ ->
                    F.fromValue F.positiveInt 0
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "0" ])
            , test "positive integer" <|
                \_ ->
                    F.fromValue F.positiveInt 1
                        |> F.toMaybe
                        |> Expect.equal (Just 1)
            , test "negative integer" <|
                \_ ->
                    F.fromValue F.positiveInt -1
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "-1" ])
            ]
        , describe "nonPositiveInt"
            [ test "zero" <|
                \_ ->
                    F.fromValue F.nonPositiveInt 0
                        |> F.toMaybe
                        |> Expect.equal (Just 0)
            , test "positive integer" <|
                \_ ->
                    F.fromValue F.nonPositiveInt 1
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "1" ])
            , test "negative integer" <|
                \_ ->
                    F.fromValue F.nonPositiveInt -1
                        |> F.toMaybe
                        |> Expect.equal (Just -1)
            ]
        , describe "negativeInt"
            [ test "zero" <|
                \_ ->
                    F.fromValue F.negativeInt 0
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "0" ])
            , test "positive integer" <|
                \_ ->
                    F.fromValue F.negativeInt 1
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "1" ])
            , test "negative integer" <|
                \_ ->
                    F.fromValue F.negativeInt -1
                        |> F.toMaybe
                        |> Expect.equal (Just -1)
            ]
        ]


floatSuite : Test
floatSuite =
    describe "Float"
        [ describe "float"
            [ fuzz Fuzz.niceFloat "float strings" <|
                \f ->
                    F.fromString F.float (String.fromFloat f)
                        |> F.toMaybe
                        |> Expect.equal (Just f)
            , test "float string surrounded by whitespace" <|
                \_ ->
                    F.fromString F.float "  3.14 \t "
                        |> F.toMaybe
                        |> Expect.equal (Just 3.14)
            , fuzz Fuzz.niceFloat "floats" <|
                \f ->
                    F.fromValue F.float f
                        |> F.toMaybe
                        |> Expect.equal (Just f)
            , fuzz Fuzz.blankString "blank strings" <|
                \s ->
                    F.fromString F.float s
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , test "non-float string" <|
                \_ ->
                    F.fromString F.int "pi"
                        |> F.toResult
                        |> Expect.equal (Err [ F.syntaxError "pi" ])
            ]
        , describe "nonNegativeFloat"
            [ test "zero" <|
                \_ ->
                    F.fromValue F.nonNegativeFloat 0
                        |> F.toMaybe
                        |> Expect.equal (Just 0)
            , test "positive float" <|
                \_ ->
                    F.fromValue F.nonNegativeFloat 0.1
                        |> F.toMaybe
                        |> Expect.equal (Just 0.1)
            , test "negative float" <|
                \_ ->
                    F.fromValue F.nonNegativeFloat -0.1
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "-0.1" ])
            ]
        , describe "positiveFloat"
            [ test "zero" <|
                \_ ->
                    F.fromValue F.positiveFloat 0
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "0" ])
            , test "positive float" <|
                \_ ->
                    F.fromValue F.positiveFloat 0.1
                        |> F.toMaybe
                        |> Expect.equal (Just 0.1)
            , test "negative float" <|
                \_ ->
                    F.fromValue F.positiveFloat -0.1
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "-0.1" ])
            ]
        , describe "nonPositiveFloat"
            [ test "zero" <|
                \_ ->
                    F.fromValue F.nonPositiveFloat 0
                        |> F.toMaybe
                        |> Expect.equal (Just 0)
            , test "positive float" <|
                \_ ->
                    F.fromValue F.nonPositiveFloat 0.1
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "0.1" ])
            , test "negative float" <|
                \_ ->
                    F.fromValue F.nonPositiveFloat -0.1
                        |> F.toMaybe
                        |> Expect.equal (Just -0.1)
            ]
        , describe "negativeFloat"
            [ test "zero" <|
                \_ ->
                    F.fromValue F.negativeFloat 0
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "0" ])
            , test "positive float" <|
                \_ ->
                    F.fromValue F.negativeFloat 0.1
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "0.1" ])
            , test "negative float" <|
                \_ ->
                    F.fromValue F.negativeFloat -0.1
                        |> F.toMaybe
                        |> Expect.equal (Just -0.1)
            ]
        ]
