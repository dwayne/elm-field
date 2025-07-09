module Dillon.CheckInView exposing (main)

--
-- Based on https://ellie-app.com/myVVqSVC2QZa1.
--

import Browser as B
import Dillon.CheckIn as CheckIn exposing (CheckIn)
import Dillon.Date as Date exposing (Date)
import Dillon.Time as Time exposing (Time)
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
    { checkIn : CheckIn
    , nameTracker : InteractionTracker.State
    , checkInTracker : InteractionTracker.State
    , checkInTimeTracker : InteractionTracker.State
    , checkOutTracker : InteractionTracker.State
    , subscribeTracker : InteractionTracker.State
    , timer : Timer
    , isSubmitting : Bool
    , maybeOutput : Maybe CheckIn.Output
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { checkIn = CheckIn.form
      , nameTracker = InteractionTracker.init
      , checkInTracker = InteractionTracker.init
      , checkInTimeTracker = InteractionTracker.init
      , checkOutTracker = InteractionTracker.init
      , subscribeTracker = InteractionTracker.init
      , timer = Timer.init
      , isSubmitting = False
      , maybeOutput = Nothing
      }
    , Cmd.none
    )


today : Date
today =
    Date.jul9th2025


timerConfig : Timer.Config Msg
timerConfig =
    Timer.config
        { wait = 5000
        , onExpire = ExpiredTimer
        , onChange = ChangedTimer
        }



-- UPDATE


type Msg
    = InputName String
    | ChangedNameTracker (InteractionTracker.Msg Msg)
    | InputCheckIn String
    | ChangedCheckInTracker (InteractionTracker.Msg Msg)
    | InputCheckInTime String
    | ChangedCheckInTimeTracker (InteractionTracker.Msg Msg)
    | InputCheckOut String
    | ChangedCheckOutTracker (InteractionTracker.Msg Msg)
    | ToggleSubscribe
    | ChangedSubscribeTracker (InteractionTracker.Msg Msg)
    | Submit
    | ExpiredTimer
    | ChangedTimer Timer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputName s ->
            ( { model | checkIn = Form.update .setName s model.checkIn }
            , Cmd.none
            )

        ChangedNameTracker subMsg ->
            let
                ( nameTracker, cmd ) =
                    InteractionTracker.update subMsg model.nameTracker
            in
            ( { model | nameTracker = nameTracker }
            , cmd
            )

        InputCheckIn s ->
            ( { model | checkIn = Form.update .setCheckIn { today = today, s = s } model.checkIn }
            , Cmd.none
            )

        ChangedCheckInTracker subMsg ->
            let
                ( checkInTracker, cmd ) =
                    InteractionTracker.update subMsg model.checkInTracker
            in
            ( { model | checkInTracker = checkInTracker }
            , cmd
            )

        InputCheckInTime s ->
            ( { model | checkIn = Form.update .setCheckInTime s model.checkIn }
            , Cmd.none
            )

        ChangedCheckInTimeTracker subMsg ->
            let
                ( checkInTimeTracker, cmd ) =
                    InteractionTracker.update subMsg model.checkInTimeTracker
            in
            ( { model | checkInTimeTracker = checkInTimeTracker }
            , cmd
            )

        InputCheckOut s ->
            ( { model | checkIn = Form.update .setCheckOut s model.checkIn }
            , Cmd.none
            )

        ChangedCheckOutTracker subMsg ->
            let
                ( checkOutTracker, cmd ) =
                    InteractionTracker.update subMsg model.checkOutTracker
            in
            ( { model | checkOutTracker = checkOutTracker }
            , cmd
            )

        ToggleSubscribe ->
            let
                { subscribe } =
                    Form.toFields model.checkIn

                b =
                    subscribe
                        |> F.toMaybe
                        |> Maybe.map not
                        |> Maybe.withDefault False
            in
            ( { model | checkIn = Form.update .setSubscribe b model.checkIn }
            , Cmd.none
            )

        ChangedSubscribeTracker subMsg ->
            let
                ( subscribeTracker, cmd ) =
                    InteractionTracker.update subMsg model.subscribeTracker
            in
            ( { model | subscribeTracker = subscribeTracker }
            , cmd
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
                | checkIn = CheckIn.form
                , nameTracker = InteractionTracker.init
                , checkInTracker = InteractionTracker.init
                , checkInTimeTracker = InteractionTracker.init
                , checkOutTracker = InteractionTracker.init
                , timer = Timer.init
                , isSubmitting = False
                , maybeOutput = Form.validateAsMaybe model.checkIn
              }
            , Cmd.none
            )

        ChangedTimer subMsg ->
            ( model
            , Timer.update timerConfig subMsg model.timer
            )



-- VIEW


view : Model -> H.Html Msg
view { checkIn, nameTracker, checkInTracker, checkInTimeTracker, checkOutTracker, subscribeTracker, timer, isSubmitting, maybeOutput } =
    let
        fields =
            Form.toFields checkIn
    in
    H.div []
        [ H.h2 [] [ H.text "Check In" ]
        , H.form
            [ HA.novalidate True
            , HE.onSubmit Submit
            ]
            [ viewInteractiveInput
                { id = "name"
                , label = "Name"
                , type_ = "text"
                , field = fields.name
                , tracker = nameTracker
                , errorToString = CheckIn.nameErrorToString
                , isRequired = True
                , isDisabled = isSubmitting
                , attrs = [ HA.autofocus True ]
                , onInput = InputName
                , onChange = ChangedNameTracker
                }
            , viewInteractiveInput
                { id = "checkIn"
                , label = "Check In"
                , type_ = "date"
                , field = fields.checkIn
                , tracker = checkInTracker
                , errorToString = Date.errorToString "check in"
                , isRequired = True
                , isDisabled = isSubmitting
                , attrs =
                    let
                        maybeMax =
                            fields.checkOut
                                |> F.toMaybe
                                |> Maybe.map (Date.toString << Date.add -1)
                    in
                    [ HA.min (Date.toString <| Date.add 1 today)
                    ]
                        ++ (case maybeMax of
                                Just max ->
                                    [ HA.max max ]

                                Nothing ->
                                    []
                           )
                , onInput = InputCheckIn
                , onChange = ChangedCheckInTracker
                }
            , viewInteractiveInput
                { id = "checkInTime"
                , label = "Check In Time"
                , type_ = "time"
                , field = fields.checkInTime
                , tracker = checkInTimeTracker
                , errorToString = Time.errorToString "check in"
                , isRequired = True
                , isDisabled = isSubmitting
                , attrs = []
                , onInput = InputCheckInTime
                , onChange = ChangedCheckInTimeTracker
                }
            , viewInteractiveInput
                { id = "checkOut"
                , label = "Check Out"
                , type_ = "date"
                , field = fields.checkOut
                , tracker = checkOutTracker
                , errorToString = Date.errorToString "check out"
                , isRequired = True
                , isDisabled = isSubmitting
                , attrs =
                    [ fields.checkIn
                        |> F.toMaybe
                        |> Maybe.map (Date.add 1)
                        |> Maybe.withDefault (Date.add 2 today)
                        |> Date.toString
                        |> HA.min
                    ]
                , onInput = InputCheckOut
                , onChange = ChangedCheckOutTracker
                }
            , viewCheckbox
                { id = "subscribe"
                , label = "Sign Up for Email Updates"
                , field = fields.subscribe
                , tracker = subscribeTracker
                , isRequired = True
                , isDisabled = isSubmitting
                , attrs = []
                , onToggle = ToggleSubscribe
                , onChange = ChangedSubscribeTracker
                }
            , H.p []
                [ H.button
                    [ HA.disabled (Form.isInvalid checkIn || isSubmitting) ]
                    [ H.text <|
                        if isSubmitting then
                            "Checking In..."

                        else
                            "Check In"
                    ]
                ]
            ]
        , case maybeOutput of
            Just { name, stay, isSubscribed } ->
                H.div []
                    [ H.h2 [] [ H.text "Output" ]
                    , H.p [] [ H.text <| "Name: " ++ name ]
                    , H.p [] [ H.text <| "Nights: " ++ String.fromInt stay.nights ]
                    , H.p [] [ H.text <| "Email Updates: " ++ boolToString isSubscribed ]
                    ]

            Nothing ->
                H.text ""
        ]


boolToString : Bool -> String
boolToString b =
    if b then
        "True"

    else
        "False"


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
        , debug = False
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


viewCheckbox :
    { id : String
    , label : String
    , field : Field Bool
    , tracker : InteractionTracker.State
    , isRequired : Bool
    , isDisabled : Bool
    , attrs : List (H.Attribute msg)
    , onToggle : msg
    , onChange : InteractionTracker.Msg msg -> msg
    }
    -> H.Html msg
viewCheckbox { id, label, field, tracker, isRequired, isDisabled, attrs, onToggle, onChange } =
    let
        checked =
            field
                |> F.toMaybe
                |> Maybe.withDefault False
    in
    viewInteractiveInput
        { id = id
        , label = label
        , type_ = "checkbox"
        , field = field
        , tracker = tracker
        , errorToString = always "An unexpected error occurred."
        , isRequired = isRequired
        , isDisabled = isDisabled
        , attrs = attrs ++ [ HA.checked checked ]
        , onInput = always onToggle
        , onChange = onChange
        }
