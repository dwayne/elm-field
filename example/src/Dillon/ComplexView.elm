module Dillon.ComplexView exposing (main)

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
    , maybeOutput : Maybe SignUp.Output
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { signUp = SignUp.form
      , usernameTracker = InteractionTracker.init
      , maybeOutput = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputUsername String
    | ChangedUsernameTracker (InteractionTracker.Msg Msg)
    | InputPassword String
    | InputPasswordConfirmation String
    | InputRole Role
    | Submit


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

        InputPasswordConfirmation s ->
            ( { model | signUp = Form.update .setPasswordConfirmation s model.signUp }
            , Cmd.none
            )

        InputRole role ->
            ( { model | signUp = Form.update .setRole role model.signUp }
            , Cmd.none
            )

        Submit ->
            ( { model | maybeOutput = Form.validateAsMaybe model.signUp }
            , Cmd.none
            )



-- VIEW


view : Model -> H.Html Msg
view { signUp, usernameTracker, maybeOutput } =
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
                , isDisabled = False
                , onInput = InputUsername
                , onChange = ChangedUsernameTracker
                }
            , viewInput
                { id = "password"
                , label = "Password"
                , type_ = "password"
                , field = fields.password
                , errorToString = Password.errorToString
                , isRequired = True
                , isDisabled = False
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


viewInteractiveInput :
    { id : String
    , label : String
    , type_ : String
    , field : Field a
    , tracker : InteractionTracker.State
    , errorToString : Error -> String
    , isRequired : Bool
    , isDisabled : Bool
    , onInput : String -> msg
    , onChange : InteractionTracker.Msg msg -> msg
    }
    -> H.Html msg
viewInteractiveInput { id, label, type_, field, tracker, errorToString, isRequired, isDisabled, onInput, onChange } =
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
                , attrs = []
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


viewInput :
    { id : String
    , label : String
    , type_ : String
    , field : Field a
    , errorToString : Error -> String
    , isRequired : Bool
    , isDisabled : Bool
    , onInput : String -> msg
    }
    -> H.Html msg
viewInput { id, label, type_, field, errorToString, isRequired, isDisabled, onInput } =
    H.p []
        [ H.label [ HA.for id ] [ H.text (label ++ ": ") ]
        , H.text " "
        , Input.view
            { field = field
            , isRequired = isRequired
            , isDisabled = isDisabled
            , onInput = onInput
            , attrs = [ HA.id id, HA.type_ type_ ]
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
