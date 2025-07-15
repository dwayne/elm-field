module Test.Field exposing (suite)

import Expect
import Field as F
import Fuzz
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Field"
        [ intSuite
        ]


intSuite : Test
intSuite =
    describe "Int"
        [ describe "int"
            [ fuzz Fuzz.int "all integer strings in the range Random.minInt to Random.maxInt inclusive" <|
                \n ->
                    F.fromString F.int (String.fromInt n)
                        |> F.toMaybe
                        |> Expect.equal (Just n)
            , fuzz Fuzz.int "all integers in the range Random.minInt to Random.maxInt inclusive" <|
                \n ->
                    F.fromValue F.int n
                        |> F.toMaybe
                        |> Expect.equal (Just n)
            ]
        ]
