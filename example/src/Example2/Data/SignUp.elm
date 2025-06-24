module Example2.Data.SignUp exposing
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

import Example2.Data.Email as Email exposing (Email)
import Example2.Data.Password as Password exposing (Password)
import Example2.Data.PasswordConfirmation as PasswordConfirmation exposing (PasswordConfirmation)
import Example2.Data.Username as Username exposing (Username)
import Field.Advanced as F


type SignUp
    = SignUp Fields


type alias Fields =
    { username : F.Field Username.Error Username
    , email : F.Field Email.Error Email
    , password : F.Field Password.Error Password
    , passwordConfirmation : F.Field PasswordConfirmation.Error PasswordConfirmation
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
    F.Field Password.Error Password
    -> F.Field PasswordConfirmation.Error PasswordConfirmation
    -> F.Field PasswordConfirmation.Error PasswordConfirmation
updatePasswordConfirmation passwordField passwordConfirmationField =
    (\password passwordConfirmation ->
        if Password.toString password == PasswordConfirmation.toString passwordConfirmation then
            passwordConfirmationField

        else
            F.setError PasswordConfirmation.Mismatch passwordConfirmationField
    )
        |> Just
        |> F.applyMaybe passwordField
        |> F.applyMaybe passwordConfirmationField
        |> Maybe.withDefault passwordConfirmationField


type Error
    = UsernameError Username.Error
    | EmailError Email.Error
    | PasswordError Password.Error
    | PasswordConfirmationError PasswordConfirmation.Error


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
        |> F.get (fields.username |> F.mapError UsernameError)
        |> F.and (fields.email |> F.mapError EmailError)
        |> F.and (fields.password |> F.mapError PasswordError)
        |> F.and (fields.passwordConfirmation |> F.mapError PasswordConfirmationError)
        |> F.andResult


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
