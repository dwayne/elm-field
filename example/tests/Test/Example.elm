module Test.Example exposing (suite)

import Example as E
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Example"
        [ describe "hello"
            [ test "it returns hello" <|
                \_ ->
                    E.hello
                        |> Expect.equal "Hello, world!"
            ]
        ]
