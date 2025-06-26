module Debouncing exposing (main)

import Browser as B
import Browser.Dom as BD
import Example1.Data.Username as Username exposing (Username)
import Field as F
import Html as H
import Html.Attributes as HA
import Lib.DebouncedField as DF
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
    , username : DebouncedField Username
    , timer : Timer
    }


type Status
    = Normal
    | Checking Username
    | Success
    | Error


type alias DebouncedField a =
    DF.DebouncedField F.Error a


init : () -> ( Model, Cmd msg )
init _ =
    ( { status = Normal
      , username = DF.fromField (F.empty Username.fieldType)
      , timer = Timer.init
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputUsername
    | ReadyToCheck Username
    | ChangedUsername DF.Msg
    | TimerExpired
    | ChangedTimer Timer.Msg
    | GotResult Bool
    | FocusUsername


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputUsername ->
            ( { model
                | status = Normal
                , timer =
                    case model.status of
                        Checking _ ->
                            Timer.cancel model.timer

                        _ ->
                            model.timer
              }
            , Cmd.none
            )

        ReadyToCheck username ->
            let
                ( timer, cmd ) =
                    Timer.setTimeout timerConfig model.timer
            in
            ( { model | status = Checking username, timer = timer }
            , cmd
            )

        ChangedUsername subMsg ->
            let
                ( username, cmd ) =
                    DF.update usernameConfig subMsg model.username
            in
            ( { model | username = username }
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

        FocusUsername ->
            ( model, Cmd.none )


focus : String -> msg -> Cmd msg
focus id msg =
    BD.focus id
        |> Task.attempt (always msg)


usernameConfig : DF.Config Username Msg
usernameConfig =
    DF.trailing
        { wait = 500
        , onInput = always InputUsername
        , onReady = ReadyToCheck
        , onChange = ChangedUsername
        }


timerConfig : Timer.Config Msg
timerConfig =
    Timer.config
        { wait = 5000
        , onExpire = TimerExpired
        , onChange = ChangedTimer
        }



-- VIEW


view : Model -> H.Html Msg
view model =
    H.form [ HA.style "margin" "10px" ]
        [ H.label [ HA.for "username" ] [ H.text "Username: " ]
        , DF.view
            { field = model.username
            , isRequired = True
            , isDisabled =
                case model.status of
                    Checking _ ->
                        True

                    _ ->
                        False
            , config = usernameConfig
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
