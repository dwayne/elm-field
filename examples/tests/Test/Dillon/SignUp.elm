module Test.Dillon.SignUp exposing (suite)

import Dillon.Role as Role
import Dillon.SignUp as SignUp
import Example1.Password as Password
import Example1.PasswordConfirmation as PasswordConfirmation
import Example1.Username as Username
import Expect
import Field as F
import Lib.Form as Form
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Dillon.SignUp"
        [ test "all the fields are correctly initialized and clean" <|
            \_ ->
                let
                    expectIsCleanAndGood toField isGood fields =
                        let
                            field =
                                toField fields
                        in
                        Expect.equal True (F.isClean field && isGood field)
                in
                SignUp.form
                    |> Form.toFields
                    |> Expect.all
                        [ expectIsCleanAndGood .username (F.toMaybe >> Maybe.map Username.toString >> (==) (Just "dillon"))
                        , expectIsCleanAndGood .password F.isEmpty
                        , expectIsCleanAndGood .passwordConfirmation F.isEmpty
                        , expectIsCleanAndGood .role (F.toMaybe >> (==) (Just Role.SuperAdmin))
                        ]
        , describe "username"
            [ test "it is required" <|
                \_ ->
                    SignUp.form
                        |> Form.update .setUsername ""
                        |> Form.toFields
                        |> .username
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , test "it must have at least 3 characters" <|
                \_ ->
                    SignUp.form
                        |> Form.update .setUsername "ab"
                        |> Form.toFields
                        |> .username
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError "The username must have at least 3 characters." ])
            , test "it must have at most 25 characters" <|
                \_ ->
                    SignUp.form
                        |> Form.update .setUsername "abcdefghijklmnopqrstuvwxyz"
                        |> Form.toFields
                        |> .username
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError "The username must have at most 25 characters." ])
            , test "it is valid" <|
                \_ ->
                    SignUp.form
                        |> Form.update .setUsername "abc"
                        |> Form.toFields
                        |> .username
                        |> F.isValid
                        |> Expect.equal True
            , test "it is trimmed" <|
                \_ ->
                    SignUp.form
                        |> Form.update .setUsername " abc "
                        |> Form.toFields
                        |> .username
                        |> F.toMaybe
                        |> Maybe.map Username.toString
                        |> Expect.equal (Just "abc")
            ]
        , describe "password"
            [ test "it is required" <|
                \_ ->
                    SignUp.form
                        |> Form.update .setPassword ""
                        |> Form.toFields
                        |> .password
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , test "it must have at least 8 characters" <|
                \_ ->
                    SignUp.form
                        |> Form.update .setPassword "1234567"
                        |> Form.toFields
                        |> .password
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError "The password must have at least 8 characters." ])
            , test "it must have the required characters" <|
                \_ ->
                    SignUp.form
                        |> Form.update .setPassword "12345678"
                        |> Form.toFields
                        |> .password
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError "The password must contain at least 1 of each of the following: a lowercase character, an uppercase character, a number, and a special character in the set \"(!@#$%^&*)\"." ])
            , test "it is valid" <|
                \_ ->
                    SignUp.form
                        |> Form.update .setPassword "12345678aB@"
                        |> Form.toFields
                        |> .password
                        |> F.isValid
                        |> Expect.equal True
            , test "it is trimmed" <|
                \_ ->
                    SignUp.form
                        |> Form.update .setPassword " 12345678aB@ "
                        |> Form.toFields
                        |> .password
                        |> F.toMaybe
                        |> Maybe.map Password.toString
                        |> Expect.equal (Just "12345678aB@")
            ]
        , describe "password confirmation"
            [ test "it is required" <|
                \_ ->
                    SignUp.form
                        |> Form.update .setPasswordConfirmation ""
                        |> Form.toFields
                        |> .passwordConfirmation
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , test "it must match the password" <|
                \_ ->
                    SignUp.form
                        |> Form.update .setPassword "12345678aB@"
                        |> Form.update .setPasswordConfirmation "1234567"
                        |> Form.toFields
                        |> .passwordConfirmation
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError "The password confirmation does not match." ])
            , test "it is valid" <|
                \_ ->
                    SignUp.form
                        |> Form.update .setPassword "12345678aB@"
                        |> Form.update .setPasswordConfirmation "12345678aB@"
                        |> Form.toFields
                        |> .passwordConfirmation
                        |> F.isValid
                        |> Expect.equal True
            , test "it is trimmed" <|
                \_ ->
                    SignUp.form
                        |> Form.update .setPassword "12345678aB@"
                        |> Form.update .setPasswordConfirmation " 12345678aB@ "
                        |> Form.toFields
                        |> .passwordConfirmation
                        |> F.toMaybe
                        |> Maybe.map PasswordConfirmation.toString
                        |> Expect.equal (Just "12345678aB@")
            ]

        --
        -- N.B. There's no way to mess up setting the role.
        --
        ]
