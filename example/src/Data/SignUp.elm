module Data.SignUp exposing
    ( Error(..)
    , SignUp
    , Submission
    , init
    , setEmail
    , setPassword
    , setPasswordConfirmation
    , setUsername
    , submit
    , toFields
    )

import Data.Email as Email exposing (Email)
import Data.Password as Password exposing (Password)
import Data.PasswordConfirmation as PasswordConfirmation
import Data.Username as Username exposing (Username)
import Field as F


type SignUp
    = SignUp Fields


type alias Fields =
    { username : F.Field (F.Error Username.Error) Username
    , email : F.Field (F.Error ()) Email
    , password : F.Field (F.Error Password.Error) Password
    , passwordConfirmation : F.Field (F.Error PasswordConfirmation.Error) Password
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


updatePasswordConfirmation :
    F.Field (F.Error Password.Error) Password
    -> F.Field (F.Error PasswordConfirmation.Error) Password
    -> F.Field (F.Error PasswordConfirmation.Error) Password
updatePasswordConfirmation passwordField passwordConfirmationField =
    (\password passwordConfirmation ->
        if password == passwordConfirmation then
            passwordConfirmationField

        else
            F.fail (F.CustomError PasswordConfirmation.Mismatch) passwordConfirmationField
    )
        |> Just
        |> F.applyMaybe passwordField
        |> F.applyMaybe passwordConfirmationField
        |> Maybe.withDefault passwordConfirmationField


type Error
    = UsernameError (F.Error Username.Error)
    | EmailError (F.Error ())
    | PasswordError (F.Error Password.Error)
    | PasswordConfirmationError (F.Error PasswordConfirmation.Error)


type alias Submission =
    { username : Username
    , email : Email
    , password : Password
    }


submit : SignUp -> Result (List Error) Submission
submit (SignUp fields) =
    Submission
        |> F.get (fields.username |> F.mapError UsernameError)
        |> F.and (fields.email |> F.mapError EmailError)
        |> F.and (fields.password |> F.mapError PasswordError)
        |> F.andIgnore (fields.passwordConfirmation |> F.mapError PasswordConfirmationError)
        |> F.andResult


toFields : SignUp -> Fields
toFields (SignUp fields) =
    fields
