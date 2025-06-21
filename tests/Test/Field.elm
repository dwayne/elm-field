module Test.Field exposing (suite)

import Expect
import Field as F
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Field"
        [ test "1 + 1" <|
            \_ ->
                (1 + 1) |> Expect.equal 2
        ]
