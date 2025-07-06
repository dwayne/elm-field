module Dillon.SignUp exposing (main)

--
-- Based on https://ellie-app.com/myVVqSVC2QZa1.
--

import Browser as B
import Field as F exposing (Field)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Lib.Input as Input
import Lib.InteractionTracker as InteractionTracker
import Lib.InteractiveInput as InteractiveInput


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
    { username : String
    , usernameTracker : InteractionTracker.State
    , usernameField : Field String
    , usernameFieldTracker : InteractionTracker.State
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { username = "dillon"
      , usernameTracker = InteractionTracker.init
      , usernameField = F.fromString F.nonBlankString "dillon"
      , usernameFieldTracker = InteractionTracker.init
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputUsername String
    | ChangedUsernameTracker (InteractionTracker.Msg Msg)
    | InputUsernameField String
    | ChangedUsernameFieldTracker (InteractionTracker.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputUsername s ->
            ( { model | username = s }
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

        InputUsernameField s ->
            ( { model | usernameField = F.setFromString s model.usernameField }
            , Cmd.none
            )

        ChangedUsernameFieldTracker subMsg ->
            let
                ( usernameFieldTracker, cmd ) =
                    InteractionTracker.update subMsg model.usernameFieldTracker
            in
            ( { model | usernameFieldTracker = usernameFieldTracker }
            , cmd
            )



-- VIEW


view : Model -> H.Html Msg
view { username, usernameTracker, usernameField, usernameFieldTracker } =
    H.div []
        [ H.h2 [] [ H.text "Sign Up" ]
        , H.form []
            [ InteractiveInput.view
                { id = "username"
                , label = "Username"
                , field = username
                , tracker = usernameTracker
                , toInput = toRegularInput InputUsername
                , onChange = ChangedUsernameTracker
                }
            , InteractiveInput.view
                { id = "usernameField"
                , label = "Username Field"
                , field = usernameField
                , tracker = usernameFieldTracker
                , toInput =
                    toFieldInput
                        { isRequired = True
                        , isDisabled = False
                        , onInput = InputUsernameField
                        , attrs = []
                        }
                , onChange = ChangedUsernameFieldTracker
                }
            ]
        ]


toRegularInput : (String -> msg) -> InteractiveInput.InputOptions String msg -> H.Html msg
toRegularInput onInput { id, field, focus, input, blur } =
    H.input
        [ HA.id id
        , HA.value field
        , HE.onFocus focus
        , HE.onInput (input onInput)
        , HE.onBlur blur
        ]
        []


toFieldInput :
    { isRequired : Bool
    , isDisabled : Bool
    , onInput : String -> msg
    , attrs : List (H.Attribute msg)
    }
    -> InteractiveInput.InputOptions (Field a) msg
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
