module Example1.SignUp exposing
    ( Error(..)
    , Fields
    , SignUp
    , Submission
    , errorToString
    , init
    , setEmail
    , setPassword
    , setPasswordConfirmation
    , setUsername
    , submit
    , toFields
    )

import Example1.Email as Email exposing (Email)
import Example1.Password as Password exposing (Password)
import Example1.PasswordConfirmation as PasswordConfirmation exposing (PasswordConfirmation)
import Example1.Username as Username exposing (Username)
import Field as F exposing (Field)


type SignUp
    = SignUp Fields


type alias Fields =
    { username : Field Username
    , email : Field Email
    , password : Field Password
    , passwordConfirmation : Field PasswordConfirmation
    }


init : SignUp
init =
    SignUp
        { username = F.empty Username.fieldType
        , email = F.empty Email.fieldType
        , password = F.empty Password.fieldType
        , passwordConfirmation = F.empty PasswordConfirmation.fieldType
        }


setUsername : String -> SignUp -> SignUp
setUsername s (SignUp fields) =
    SignUp { fields | username = F.setFromString s fields.username }


setEmail : String -> SignUp -> SignUp
setEmail s (SignUp fields) =
    SignUp { fields | email = F.setFromString s fields.email }


setPassword : String -> SignUp -> SignUp
setPassword s (SignUp fields) =
    let
        password =
            F.setFromString s fields.password
    in
    SignUp { fields | password = password, passwordConfirmation = updatePasswordConfirmation password fields.passwordConfirmation }


setPasswordConfirmation : String -> SignUp -> SignUp
setPasswordConfirmation s (SignUp fields) =
    let
        passwordConfirmation =
            F.setFromString s fields.passwordConfirmation
    in
    SignUp { fields | passwordConfirmation = updatePasswordConfirmation fields.password passwordConfirmation }


updatePasswordConfirmation : Field Password -> Field PasswordConfirmation -> Field PasswordConfirmation
updatePasswordConfirmation passwordField passwordConfirmationField =
    (\password passwordConfirmation ->
        if Password.toString password == PasswordConfirmation.toString passwordConfirmation then
            passwordConfirmationField

        else
            F.setCustomError "The password confirmation does not match." passwordConfirmationField
    )
        |> Just
        |> F.applyMaybe passwordField
        |> F.applyMaybe passwordConfirmationField
        |> Maybe.withDefault passwordConfirmationField


type Error
    = UsernameError F.Error
    | EmailError F.Error
    | PasswordError F.Error
    | PasswordConfirmationError F.Error


type alias Submission =
    { username : Username
    , email : Email
    , password : Password
    }


submit : SignUp -> Result (List Error) Submission
submit (SignUp fields) =
    (\username email password _ ->
        Submission username email password
    )
        |> F.succeed (fields.username |> F.mapError UsernameError)
        |> F.applyValidation (fields.email |> F.mapError EmailError)
        |> F.applyValidation (fields.password |> F.mapError PasswordError)
        |> F.applyValidation (fields.passwordConfirmation |> F.mapError PasswordConfirmationError)
        |> F.validationToResult


errorToString : Error -> String
errorToString error =
    case error of
        UsernameError e ->
            Username.errorToString e

        EmailError e ->
            Email.errorToString e

        PasswordError e ->
            Password.errorToString e

        PasswordConfirmationError e ->
            PasswordConfirmation.errorToString e


toFields : SignUp -> Fields
toFields (SignUp fields) =
    fields
