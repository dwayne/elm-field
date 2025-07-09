module Dillon.ComplexSignUpView exposing (main)

--
-- Based on https://ellie-app.com/myVVqSVC2QZa1.
--

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
import Lib.InteractionTracker as InteractionTracker
import Lib.InteractiveInput as InteractiveInput
import Lib.Select as Select
import Lib.Selection.NonEmpty as Selection exposing (Selection)
import Lib.Timer as Timer exposing (Timer)


main : Program () Model Msg
main =
    B.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { signUp : SignUp
    , usernameTracker : InteractionTracker.State
    , passwordTracker : InteractionTracker.State
    , passwordConfirmationTracker : InteractionTracker.State
    , timer : Timer
    , isSubmitting : Bool
    , maybeOutput : Maybe SignUp.Output
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { signUp = SignUp.form
      , usernameTracker = InteractionTracker.init
      , passwordTracker = InteractionTracker.init
      , passwordConfirmationTracker = InteractionTracker.init
      , timer = Timer.init
      , isSubmitting = False
      , maybeOutput = Nothing
      }
    , Cmd.none
    )


timerConfig : Timer.Config Msg
timerConfig =
    Timer.config
        { wait = 5000
        , onExpire = ExpiredTimer
        , onChange = ChangedTimer
        }



-- UPDATE


type Msg
    = InputUsername String
    | ChangedUsernameTracker (InteractionTracker.Msg Msg)
    | InputPassword String
    | ChangedPasswordTracker (InteractionTracker.Msg Msg)
    | InputPasswordConfirmation String
    | ChangedPasswordConfirmationTracker (InteractionTracker.Msg Msg)
    | InputRole Role
    | Submit
    | ExpiredTimer
    | ChangedTimer Timer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputUsername s ->
            ( { model | signUp = Form.update .setUsername s model.signUp }
            , Cmd.none
            )

        ChangedUsernameTracker subMsg ->
            let
                ( usernameTracker, cmd ) =
                    InteractionTracker.update subMsg model.usernameTracker
            in
            ( { model | usernameTracker = usernameTracker }
            , cmd
            )

        InputPassword s ->
            ( { model | signUp = Form.update .setPassword s model.signUp }
            , Cmd.none
            )

        ChangedPasswordTracker subMsg ->
            let
                ( passwordTracker, cmd ) =
                    InteractionTracker.update subMsg model.passwordTracker
            in
            ( { model | passwordTracker = passwordTracker }
            , cmd
            )

        InputPasswordConfirmation s ->
            ( { model | signUp = Form.update .setPasswordConfirmation s model.signUp }
            , Cmd.none
            )

        ChangedPasswordConfirmationTracker subMsg ->
            let
                ( passwordConfirmationTracker, cmd ) =
                    InteractionTracker.update subMsg model.passwordConfirmationTracker
            in
            ( { model | passwordConfirmationTracker = passwordConfirmationTracker }
            , cmd
            )

        InputRole role ->
            ( { model | signUp = Form.update .setRole role model.signUp }
            , Cmd.none
            )

        Submit ->
            let
                ( timer, cmd ) =
                    Timer.setTimeout timerConfig model.timer
            in
            ( { model | timer = timer, isSubmitting = True }
            , cmd
            )

        ExpiredTimer ->
            ( { model
                | signUp = SignUp.form
                , usernameTracker = InteractionTracker.init
                , passwordTracker = InteractionTracker.init
                , passwordConfirmationTracker = InteractionTracker.init
                , timer = Timer.init
                , isSubmitting = False
                , maybeOutput = Form.validateAsMaybe model.signUp
              }
            , Cmd.none
            )

        ChangedTimer subMsg ->
            ( model
            , Timer.update timerConfig subMsg model.timer
            )



-- VIEW


view : Model -> H.Html Msg
view { signUp, usernameTracker, passwordTracker, passwordConfirmationTracker, timer, isSubmitting, maybeOutput } =
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
            [ viewInteractiveInput
                { id = "username"
                , label = "Username"
                , type_ = "text"
                , field = fields.username
                , tracker = usernameTracker
                , errorToString = Username.errorToString
                , isRequired = True
                , isDisabled = isSubmitting
                , attrs = [ HA.autofocus True ]
                , onInput = InputUsername
                , onChange = ChangedUsernameTracker
                }
            , viewInteractiveInput
                { id = "password"
                , label = "Password"
                , type_ = "password"
                , field = fields.password
                , tracker = passwordTracker
                , errorToString = Password.errorToString
                , isRequired = True
                , isDisabled = isSubmitting
                , attrs = []
                , onInput = InputPassword
                , onChange = ChangedPasswordTracker
                }
            , viewInteractiveInput
                { id = "passwordConfirmation"
                , label = "Password Confirmation"
                , type_ = "password"
                , field = fields.passwordConfirmation
                , tracker = passwordConfirmationTracker
                , errorToString = PasswordConfirmation.errorToString
                , isRequired = True
                , isDisabled = isSubmitting
                , attrs = []
                , onInput = InputPasswordConfirmation
                , onChange = ChangedPasswordConfirmationTracker
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
                , isDisabled = isSubmitting
                , onInput = InputRole
                }
            , H.p []
                [ H.button
                    [ HA.disabled (Form.isInvalid signUp || isSubmitting) ]
                    [ H.text <|
                        if isSubmitting then
                            "Signing Up..."

                        else
                            "Sign Up"
                    ]
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


viewInteractiveInput :
    { id : String
    , label : String
    , type_ : String
    , field : Field a
    , tracker : InteractionTracker.State
    , errorToString : Error -> String
    , isRequired : Bool
    , isDisabled : Bool
    , attrs : List (H.Attribute msg)
    , onInput : String -> msg
    , onChange : InteractionTracker.Msg msg -> msg
    }
    -> H.Html msg
viewInteractiveInput { id, label, type_, field, tracker, errorToString, isRequired, isDisabled, attrs, onInput, onChange } =
    InteractiveInput.view
        { id = id
        , label = label
        , field = field
        , tracker = tracker
        , errorToString = errorToString
        , toInput =
            toFieldInput
                { isRequired = isRequired
                , isDisabled = isDisabled
                , onInput = onInput
                , attrs = attrs ++ [ HA.type_ type_ ]
                }
        , onChange = onChange
        }


toFieldInput :
    { isRequired : Bool
    , isDisabled : Bool
    , onInput : String -> msg
    , attrs : List (H.Attribute msg)
    }
    -> InteractiveInput.InputOptions a msg
    -> H.Html msg
toFieldInput { isRequired, isDisabled, onInput, attrs } { id, field, focus, input, blur } =
    Input.view
        { field = field
        , isRequired = isRequired
        , isDisabled = isDisabled
        , onInput = input onInput
        , attrs =
            attrs
                ++ [ HA.id id
                   , HE.onFocus focus
                   , HE.onBlur blur
                   ]
        }


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
