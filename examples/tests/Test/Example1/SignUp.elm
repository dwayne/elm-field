module Test.Example1.SignUp exposing (suite)

import Example1.SignUp as SignUp
import Example1.Username as Username
import Expect
import Field as F
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Example1.SignUp"
        [ test "all the fields are initially empty and clean" <|
            \_ ->
                let
                    expectIsEmptyAndClean toField fields =
                        let
                            field =
                                toField fields
                        in
                        Expect.equal True (F.isEmpty field && F.isClean field)
                in
                SignUp.init
                    |> SignUp.toFields
                    |> Expect.all
                        [ expectIsEmptyAndClean .username
                        , expectIsEmptyAndClean .email
                        , expectIsEmptyAndClean .password
                        , expectIsEmptyAndClean .passwordConfirmation
                        ]
        , describe "username"
            [ test "it is required" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setUsername ""
                        |> SignUp.toFields
                        |> .username
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , test "it must have at least 3 characters" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setUsername "ab"
                        |> SignUp.toFields
                        |> .username
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError "The username must have at least 3 characters." ])
            , test "it must have at most 25 characters" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setUsername "abcdefghijklmnopqrstuvwxyz"
                        |> SignUp.toFields
                        |> .username
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError "The username must have at most 25 characters." ])
            , test "a valid username" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setUsername "abc"
                        |> SignUp.toFields
                        |> .username
                        |> F.isValid
                        |> Expect.equal True
            , test "the input is trimmed" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setUsername " abc "
                        |> SignUp.toFields
                        |> .username
                        |> F.toMaybe
                        |> Maybe.map Username.toString
                        |> Expect.equal (Just "abc")
            ]
        ]
