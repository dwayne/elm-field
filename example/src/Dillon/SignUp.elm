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
import Lib.InteractiveInput as InteractiveInput exposing (InteractiveInput)


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
    { username : InteractiveInput String
    , usernameField : InteractiveInput (Field String)
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { username = InteractiveInput.init "dillon"
      , usernameField = InteractiveInput.init <| F.fromString F.nonBlankString "dillon"
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputUsername String
    | ChangedUsername (InteractiveInput.Msg Msg)
    | InputUsernameField String
    | ChangedUsernameField (InteractiveInput.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputUsername s ->
            ( { model | username = InteractiveInput.modify (always s) model.username }
            , Cmd.none
            )

        ChangedUsername subMsg ->
            let
                ( username, cmd ) =
                    InteractiveInput.update subMsg model.username
            in
            ( { model | username = username }
            , cmd
            )

        InputUsernameField s ->
            ( { model | usernameField = InteractiveInput.modify (F.setFromString s) model.usernameField }
            , Cmd.none
            )

        ChangedUsernameField subMsg ->
            let
                ( usernameField, cmd ) =
                    InteractiveInput.update subMsg model.usernameField
            in
            ( { model | usernameField = usernameField }
            , cmd
            )



-- VIEW


view : Model -> H.Html Msg
view { username, usernameField } =
    H.div []
        [ H.h2 [] [ H.text "Sign Up" ]
        , H.form []
            [ InteractiveInput.view
                { id = "username"
                , label = "Username"
                , toInput = toRegularInput InputUsername
                , onChange = ChangedUsername
                }
                username
            , InteractiveInput.view
                { id = "usernameField"
                , label = "Username Field"
                , toInput =
                    toFieldInput
                        { isRequired = True
                        , isDisabled = False
                        , onInput = InputUsernameField
                        , attrs = []
                        }
                , onChange = ChangedUsernameField
                }
                usernameField
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
