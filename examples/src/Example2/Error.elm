module Example2.Error exposing
    ( emailErrorToString
    , passwordConfirmationErrorToString
    , passwordErrorToString
    , signUpErrorToString
    , usernameErrorToString
    )

import Example2.Email as Email
import Example2.Password as Password
import Example2.PasswordConfirmation as PasswordConfirmation
import Example2.SignUp as SignUp
import Example2.Username as Username
import Field.Advanced as F



--
-- Shows how to centralized all the error messages.
--


usernameErrorToString : Username.Error -> String
usernameErrorToString =
    F.errorToString
        { onBlank = "The username is required."
        , onSyntaxError = always ""
        , onValidationError = always ""
        , onCustomError =
            \e ->
                case e of
                    Username.TooShort _ ->
                        "The username must have at least 3 characters."

                    Username.TooLong _ ->
                        "The username must have at most 25 characters."
        }


emailErrorToString : Email.Error -> String
emailErrorToString =
    F.errorToString
        { onBlank = "The email is required."
        , onSyntaxError = always "The email is not valid."
        , onValidationError = always ""
        , onCustomError = always ""
        }


passwordErrorToString : Password.Error -> String
passwordErrorToString =
    F.errorToString
        { onBlank = "The password is required."
        , onSyntaxError = always ""
        , onValidationError = always ""
        , onCustomError =
            \e ->
                case e of
                    Password.TooShort _ ->
                        "The password must have at least 8 characters."

                    Password.MissingRequiredChars requiredChars ->
                        "The password must contain at least 1 of each of the following: a lowercase character, an uppercase character, a number, and a special character in the set \"(" ++ requiredChars ++ ")\"."
        }


passwordConfirmationErrorToString : PasswordConfirmation.Error -> String
passwordConfirmationErrorToString =
    F.errorToString
        { onBlank = "The password confirmation is required."
        , onSyntaxError = always ""
        , onValidationError = always ""
        , onCustomError = always "The password confirmation does not match."
        }


signUpErrorToString : SignUp.Error -> String
signUpErrorToString error =
    case error of
        SignUp.UsernameError e ->
            usernameErrorToString e

        SignUp.EmailError e ->
            emailErrorToString e

        SignUp.PasswordError e ->
            passwordErrorToString e

        SignUp.PasswordConfirmationError e ->
            passwordConfirmationErrorToString e
