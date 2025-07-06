module Lib.Form exposing (Config, Form, new, set, submit, toFields)

import Example2.Data.Email as Email exposing (Email)
import Example2.Data.Password as Password exposing (Password)
import Example2.Data.PasswordConfirmation as PasswordConfirmation exposing (PasswordConfirmation)
import Example2.Data.Username as Username exposing (Username)
import Field.Advanced as F



-- What's the advantage of doing it this way?


type Form name fields error output
    = Form
        { config : Config name fields error output
        , fields : fields
        }


type alias Config name fields error output =
    { init : fields
    , set : name -> String -> fields -> fields
    , submit : fields -> Result (List error) output
    }


new : Config name fields error output -> Form name fields error output
new config =
    Form
        { config = config
        , fields = config.init
        }


set : name -> String -> Form name fields error output -> Form name fields error output
set name s (Form { config, fields }) =
    Form
        { config = config
        , fields = config.set name s fields
        }


submit : Form name fields error output -> Result (List error) output
submit (Form { config, fields }) =
    config.submit fields


toFields : Form name fields error output -> fields
toFields (Form { fields }) =
    fields



-- EXAMPLE


type alias Fields =
    { username : F.Field Username.Error Username
    , email : F.Field Email.Error Email
    , password : F.Field Password.Error Password
    , passwordConfirmation : F.Field PasswordConfirmation.Error PasswordConfirmation
    }


type Error
    = UsernameError Username.Error
    | EmailError Email.Error
    | PasswordError Password.Error
    | PasswordConfirmationError PasswordConfirmation.Error


type alias Output =
    { username : Username
    , email : Email
    , password : Password
    }


signUp : Form String Fields Error Output
signUp =
    new
        { init =
            { username = F.empty Username.fieldType
            , email = F.empty Email.fieldType
            , password = F.empty Password.fieldType
            , passwordConfirmation = F.empty PasswordConfirmation.fieldType
            }
        , set =
            \name s fields ->
                case name of
                    "username" ->
                        { fields | username = F.setFromString s fields.username }

                    "email" ->
                        { fields | email = F.setFromString s fields.email }

                    "password" ->
                        let
                            password =
                                F.setFromString s fields.password
                        in
                        { fields | password = password, passwordConfirmation = updatePasswordConfirmation password fields.passwordConfirmation }

                    "passwordConfirmation" ->
                        let
                            passwordConfirmation =
                                F.setFromString s fields.passwordConfirmation
                        in
                        { fields | passwordConfirmation = updatePasswordConfirmation fields.password passwordConfirmation }

                    _ ->
                        fields
        , submit =
            \fields ->
                (\username email password _ ->
                    Output username email password
                )
                    |> F.get (fields.username |> F.mapError UsernameError)
                    |> F.and (fields.email |> F.mapError EmailError)
                    |> F.and (fields.password |> F.mapError PasswordError)
                    |> F.and (fields.passwordConfirmation |> F.mapError PasswordConfirmationError)
                    |> F.andResult
        }


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
