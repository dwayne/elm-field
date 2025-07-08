module Dillon.SimpleView exposing (main)

import Browser as B
import Dillon.Role as Role exposing (Role)
import Dillon.SignUp as SignUp exposing (SignUp)
import Example1.Data.Password as Password exposing (Password)
import Example1.Data.PasswordConfirmation as PasswordConfirmation exposing (PasswordConfirmation)
import Example1.Data.Username as Username exposing (Username)
import Field as F exposing (Error, Field)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Lib.Form as Form
import Lib.Input as Input
import Lib.Select as Select
import Lib.Selection.NonEmpty as Selection exposing (Selection)


main : Program () Model Msg
main =
    B.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { signUp : SignUp
    , maybeOutput : Maybe SignUp.Output
    }


init : Model
init =
    { signUp = SignUp.form
    , maybeOutput = Nothing
    }



-- UPDATE


type Msg
    = InputUsername String
    | InputPassword String
    | InputPasswordConfirmation String
    | InputRole Role
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputUsername s ->
            { model | signUp = Form.update .setUsername s model.signUp }

        InputPassword s ->
            { model | signUp = Form.update .setPassword s model.signUp }

        InputPasswordConfirmation s ->
            { model | signUp = Form.update .setPasswordConfirmation s model.signUp }

        InputRole role ->
            { model | signUp = Form.update .setRole role model.signUp }

        Submit ->
            { model | maybeOutput = Form.validateAsMaybe model.signUp }



-- VIEW


view : Model -> H.Html Msg
view { signUp, maybeOutput } =
    let
        fields =
            Form.toFields signUp
    in
    H.div []
        [ H.h2 [] [ H.text "Sign Up" ]
        , H.form
            [ HA.novalidate True
            , HE.onSubmit Submit
            ]
            [ viewInput
                { id = "username"
                , label = "Username"
                , type_ = "text"
                , field = fields.username
                , errorToString = Username.errorToString
                , isRequired = True
                , isDisabled = False
                , attrs = [ HA.autofocus True ]
                , onInput = InputUsername
                }
            , viewInput
                { id = "password"
                , label = "Password"
                , type_ = "password"
                , field = fields.password
                , errorToString = Password.errorToString
                , isRequired = True
                , isDisabled = False
                , attrs = []
                , onInput = InputPassword
                }
            , viewInput
                { id = "passwordConfirmation"
                , label = "Password Confirmation"
                , type_ = "password"
                , field = fields.passwordConfirmation
                , errorToString = PasswordConfirmation.errorToString
                , isRequired = True
                , isDisabled = False
                , attrs = []
                , onInput = InputPasswordConfirmation
                }
            , viewSelect
                { id = "role"
                , label = "Role"
                , field = fields.role
                , options =
                    case F.toMaybe fields.role of
                        Just role ->
                            Selection.select role defaultRoleOptions

                        Nothing ->
                            defaultRoleOptions
                , toOption = roleToString
                , errorToString = Role.errorToString
                , isRequired = True
                , isDisabled = False
                , onInput = InputRole
                }
            , H.p []
                [ H.button
                    [ HA.disabled <| Form.isInvalid signUp ]
                    [ H.text "Sign Up" ]
                ]
            ]
        , case maybeOutput of
            Just { username, password, role } ->
                H.div []
                    [ H.h2 [] [ H.text "Output" ]
                    , H.p [] [ H.text <| "Username: " ++ Username.toString username ]
                    , H.p [] [ H.text <| "Password: " ++ Password.toString password ]
                    , H.p [] [ H.text <| "Role: " ++ roleToString role ]
                    ]

            Nothing ->
                H.text ""
        ]


defaultRoleOptions : Selection Role
defaultRoleOptions =
    Selection.fromList [] Role.Admin [ Role.SuperAdmin, Role.Regular ]


roleToString : Role -> String
roleToString role =
    case role of
        Role.Regular ->
            "Regular"

        Role.Admin ->
            "Admin"

        Role.SuperAdmin ->
            "Super Admin"


viewInput :
    { id : String
    , label : String
    , type_ : String
    , field : Field a
    , errorToString : Error -> String
    , isRequired : Bool
    , isDisabled : Bool
    , attrs : List (H.Attribute msg)
    , onInput : String -> msg
    }
    -> H.Html msg
viewInput { id, label, type_, field, errorToString, isRequired, isDisabled, attrs, onInput } =
    H.p []
        [ H.label [ HA.for id ] [ H.text (label ++ ": ") ]
        , H.text " "
        , Input.view
            { field = field
            , isRequired = isRequired
            , isDisabled = isDisabled
            , onInput = onInput
            , attrs = attrs ++ [ HA.id id, HA.type_ type_ ]
            }
        , if F.isDirty field then
            field
                |> F.allErrors
                |> List.map
                    (\e ->
                        H.li [ HA.style "color" "red" ] [ H.text <| errorToString e ]
                    )
                |> H.ul []

          else
            H.text ""
        ]


viewSelect :
    { id : String
    , label : String
    , field : Field a
    , options : Selection a
    , toOption : a -> String
    , errorToString : Error -> String
    , isRequired : Bool
    , isDisabled : Bool
    , onInput : a -> msg
    }
    -> H.Html msg
viewSelect { id, label, field, options, toOption, errorToString, isRequired, isDisabled, onInput } =
    H.p []
        [ H.label [ HA.for id ] [ H.text (label ++ ": ") ]
        , H.text " "
        , Select.view
            { field = field
            , options = options
            , toOption = toOption
            , errorToString = errorToString
            , isRequired = isRequired
            , isDisabled = isDisabled
            , onInput = onInput
            , attrs = [ HA.id id ]
            , optionAttrs = always []
            }
        ]
