module Debouncing exposing (main)

import Browser as B
import Browser.Dom as BD
import Example1.Username as Username exposing (Username)
import Field as F exposing (Field)
import Html as H
import Html.Attributes as HA
import Lib.DebouncedInput as DebouncedInput exposing (DebouncedInput)
import Lib.Timer as Timer exposing (Timer)
import Random
import Task


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
    { status : Status
    , username : Field Username
    , usernameDebouncedInput : DebouncedInput
    , timer : Timer
    }


type Status
    = Normal
    | Checking Username
    | Success
    | Error


init : () -> ( Model, Cmd msg )
init _ =
    ( { status = Normal
      , username = F.empty Username.fieldType
      , usernameDebouncedInput = DebouncedInput.init
      , timer = Timer.init
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputUsername String
    | ReadyUsername String
    | FocusUsername
    | ChangedUsernameDebouncedInput DebouncedInput.Msg
    | TimerExpired
    | ChangedTimer Timer.Msg
    | GotResult Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputUsername s ->
            ( { model
                | status = Normal
                , username = F.setFromString s model.username
                , timer =
                    case model.status of
                        Checking _ ->
                            Timer.cancel model.timer

                        _ ->
                            model.timer
              }
            , Cmd.none
            )

        ReadyUsername s ->
            let
                usernameField =
                    F.setFromString s model.username

                ( timer, cmd ) =
                    Timer.setTimeout timerConfig model.timer

                newModel =
                    { model | username = usernameField, timer = timer }
            in
            ( case F.toMaybe usernameField of
                Just username ->
                    { newModel | status = Checking username }

                Nothing ->
                    newModel
            , cmd
            )

        FocusUsername ->
            ( model, Cmd.none )

        ChangedUsernameDebouncedInput subMsg ->
            let
                ( usernameDebouncedInput, cmd ) =
                    DebouncedInput.update usernameDebouncedInputConfig subMsg model.usernameDebouncedInput
            in
            ( { model | usernameDebouncedInput = usernameDebouncedInput }
            , cmd
            )

        TimerExpired ->
            ( model
            , Random.generate GotResult <|
                Random.weighted
                    ( 1, False )
                    [ ( 3, True )
                    ]
            )

        ChangedTimer timerMsg ->
            ( model
            , Timer.update timerConfig timerMsg model.timer
            )

        GotResult isFree ->
            case model.status of
                Checking _ ->
                    ( { model
                        | status =
                            if isFree then
                                Success

                            else
                                Error
                      }
                    , focus "username" FocusUsername
                    )

                _ ->
                    ( model, Cmd.none )


focus : String -> msg -> Cmd msg
focus id msg =
    BD.focus id
        |> Task.attempt (always msg)


usernameDebouncedInputConfig : DebouncedInput.Config Msg
usernameDebouncedInputConfig =
    DebouncedInput.trailing
        { wait = 500
        , onInput = InputUsername
        , onReady = ReadyUsername
        , onChange = ChangedUsernameDebouncedInput
        }


timerConfig : Timer.Config Msg
timerConfig =
    Timer.config
        { wait = 1000
        , onExpire = TimerExpired
        , onChange = ChangedTimer
        }



-- VIEW


view : Model -> H.Html Msg
view model =
    H.form [ HA.style "margin" "10px" ]
        [ H.label [ HA.for "username" ] [ H.text "Username: " ]
        , DebouncedInput.view
            { field = model.username
            , debouncedInput = model.usernameDebouncedInput
            , isRequired = True
            , isDisabled = False
            , config = usernameDebouncedInputConfig
            , attrs =
                [ HA.autofocus True
                , HA.id "username"
                ]
            }
        , H.p []
            [ case model.status of
                Normal ->
                    H.text ""

                Checking username ->
                    H.em
                        []
                        [ H.text <| "Checking if \"" ++ Username.toString username ++ "\" is free..."
                        ]

                Success ->
                    H.span
                        [ HA.style "color" "green" ]
                        [ H.text "The username is available."
                        ]

                Error ->
                    H.span
                        [ HA.style "color" "red" ]
                        [ H.text "The username is already taken."
                        ]
            ]
        ]
