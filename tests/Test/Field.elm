module Test.Field exposing (suite)

import Expect
import Field as F
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, test)
import Test.Lib.Fuzz as Fuzz


suite : Test
suite =
    describe "Field"
        [ intSuite
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
        ]
