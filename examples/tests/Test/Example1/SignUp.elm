module Test.Example1.SignUp exposing (suite)

import Example1.Email as Email
import Example1.Password as Password
import Example1.PasswordConfirmation as PasswordConfirmation
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
            , test "it is valid" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setUsername "abc"
                        |> SignUp.toFields
                        |> .username
                        |> F.isValid
                        |> Expect.equal True
            , test "it is trimmed" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setUsername " abc "
                        |> SignUp.toFields
                        |> .username
                        |> F.toMaybe
                        |> Maybe.map Username.toString
                        |> Expect.equal (Just "abc")
            ]
        , describe "email"
            [ test "it is required" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setEmail ""
                        |> SignUp.toFields
                        |> .email
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , test "it must contain the @ symbol" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setEmail "name at domain dot com"
                        |> SignUp.toFields
                        |> .email
                        |> F.toResult
                        |> Expect.equal (Err [ F.syntaxError "name at domain dot com" ])
            , test "it is valid" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setEmail "name@domain.com"
                        |> SignUp.toFields
                        |> .email
                        |> F.isValid
                        |> Expect.equal True
            , test "it is trimmed" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setEmail " name@domain.com "
                        |> SignUp.toFields
                        |> .email
                        |> F.toMaybe
                        |> Maybe.map Email.toString
                        |> Expect.equal (Just "name@domain.com")
            ]
        , describe "password"
            [ test "it is required" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setPassword ""
                        |> SignUp.toFields
                        |> .password
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , test "it must have at least 8 characters" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setPassword "1234567"
                        |> SignUp.toFields
                        |> .password
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError "The password must have at least 8 characters." ])
            , test "it must have the required characters" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setPassword "12345678"
                        |> SignUp.toFields
                        |> .password
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError "The password must contain at least 1 of each of the following: a lowercase character, an uppercase character, a number, and a special character in the set \"(!@#$%^&*)\"." ])
            , test "it is valid" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setPassword "12345678aB@"
                        |> SignUp.toFields
                        |> .password
                        |> F.isValid
                        |> Expect.equal True
            , test "it is trimmed" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setPassword " 12345678aB@ "
                        |> SignUp.toFields
                        |> .password
                        |> F.toMaybe
                        |> Maybe.map Password.toString
                        |> Expect.equal (Just "12345678aB@")
            ]
        , describe "password confirmation"
            [ test "it is required" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setPasswordConfirmation ""
                        |> SignUp.toFields
                        |> .passwordConfirmation
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , test "it must match the password" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setPassword "12345678aB@"
                        |> SignUp.setPasswordConfirmation "1234567"
                        |> SignUp.toFields
                        |> .passwordConfirmation
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError "The password confirmation does not match." ])
            , test "it is valid" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setPassword "12345678aB@"
                        |> SignUp.setPasswordConfirmation "12345678aB@"
                        |> SignUp.toFields
                        |> .passwordConfirmation
                        |> F.isValid
                        |> Expect.equal True
            , test "it is trimmed" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setPassword "12345678aB@"
                        |> SignUp.setPasswordConfirmation " 12345678aB@ "
                        |> SignUp.toFields
                        |> .passwordConfirmation
                        |> F.toMaybe
                        |> Maybe.map PasswordConfirmation.toString
                        |> Expect.equal (Just "12345678aB@")
            ]
        ]
