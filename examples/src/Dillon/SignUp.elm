module Dillon.SignUp exposing
    ( Error
    , Fields
    , Output
    , Setters
    , SignUp
    , form
    )

import Dillon.Role as Role exposing (Role)
import Example1.Password as Password exposing (Password)
import Example1.PasswordConfirmation as PasswordConfirmation exposing (PasswordConfirmation)
import Example1.Username as Username exposing (Username)
import Field as F exposing (Field)
import Lib.Form as Form exposing (Form)
import Validation exposing (Validation)



-- FORM


type alias SignUp =
    Form Fields Setters Error Output


type alias Fields =
    { username : Field Username
    , password : Field Password
    , passwordConfirmation : Field PasswordConfirmation
    , role : Field Role
    }


type alias Setters =
    { setUsername : String -> Fields -> Fields
    , setPassword : String -> Fields -> Fields
    , setPasswordConfirmation : String -> Fields -> Fields
    , setRole : Role -> Fields -> Fields
    }


type Error
    = UsernameError F.Error
    | PasswordError F.Error
    | PasswordConfirmationError F.Error
    | RoleError F.Error


type alias Output =
    { username : Username
    , password : Password
    , role : Role
    }


form : SignUp
form =
    Form.new
        { init = init
        , setters = setters
        , validate = validate
        }



-- INIT


init : Fields
init =
    { username = F.fromString Username.fieldType "dillon"
    , password = F.empty Password.fieldType
    , passwordConfirmation = F.empty PasswordConfirmation.fieldType
    , role = F.fromValue Role.fieldType Role.SuperAdmin
    }



-- SETTERS


setters : Setters
setters =
    { setUsername =
        \s fields ->
            { fields | username = F.setFromString s fields.username }
    , setPassword =
        \s fields ->
            let
                password =
                    F.setFromString s fields.password
            in
            { fields | password = password, passwordConfirmation = updatePasswordConfirmation password fields.passwordConfirmation }
    , setPasswordConfirmation =
        \s fields ->
            let
                passwordConfirmation =
                    F.setFromString s fields.passwordConfirmation
            in
            { fields | passwordConfirmation = updatePasswordConfirmation fields.password passwordConfirmation }
    , setRole =
        \role fields ->
            { fields | role = F.setFromValue role fields.role }
    }


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



-- VALIDATE


validate : Fields -> Validation Error Output
validate fields =
    (\username password _ role ->
        Output username password role
    )
        |> F.succeed
        |> F.applyValidation (fields.username |> F.mapError UsernameError)
        |> F.applyValidation (fields.password |> F.mapError PasswordError)
        |> F.applyValidation (fields.passwordConfirmation |> F.mapError PasswordConfirmationError)
        |> F.applyValidation (fields.role |> F.mapError RoleError)
