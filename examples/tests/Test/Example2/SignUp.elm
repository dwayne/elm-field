module Test.Example2.SignUp exposing (suite)

import Example2.Email as Email
import Example2.Password as Password
import Example2.PasswordConfirmation as PasswordConfirmation
import Example2.SignUp as SignUp
import Example2.Username as Username
import Expect
import Field.Advanced as F
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Example2.SignUp"
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
                        |> Expect.equal (Err [ F.customError (Username.TooShort 2) ])
            , test "it must have at most 25 characters" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setUsername "abcdefghijklmnopqrstuvwxyz"
                        |> SignUp.toFields
                        |> .username
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError (Username.TooLong 26) ])
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
                        |> Expect.equal (Err [ F.customError (Password.TooShort 7) ])
            , test "it must have the required characters" <|
                \_ ->
                    SignUp.init
                        |> SignUp.setPassword "12345678"
                        |> SignUp.toFields
                        |> .password
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError (Password.MissingRequiredChars "!@#$%^&*") ])
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
                        |> Expect.equal (Err [ F.customError PasswordConfirmation.Mismatch ])
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
