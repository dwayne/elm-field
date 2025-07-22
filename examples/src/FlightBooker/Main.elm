module FlightBooker.Main exposing (main)

import Browser as B
import Field as F exposing (Error, Field)
import FlightBooker.Date as Date exposing (Date)
import FlightBooker.Flight as Flight exposing (Flight)
import FlightBooker.Form as FlightBooker
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Lib.Form as Form
import Lib.Input as Input
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
    { today : Date
    , flightBooker : FlightBooker.Form
    , timer : Timer
    , isSubmitting : Bool
    , maybeTicket : Maybe FlightBooker.Ticket
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        today =
            Date.default
    in
    ( { today = today
      , flightBooker = FlightBooker.form today
      , timer = Timer.init
      , isSubmitting = False
      , maybeTicket = Nothing
      }
    , Date.today GotDate
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
    = GotDate Date
    | InputFlight Flight
    | InputDeparture String
    | InputReturn String
    | Submit
    | ExpiredTimer
    | ChangedTimer Timer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDate date ->
            ( { model | today = date, flightBooker = FlightBooker.form date }
            , Cmd.none
            )

        InputFlight flight ->
            ( { model | flightBooker = Form.update .setFlight { today = model.today, flight = flight } model.flightBooker }
            , Cmd.none
            )

        InputDeparture s ->
            ( { model | flightBooker = Form.update .setDeparture s model.flightBooker }
            , Cmd.none
            )

        InputReturn s ->
            ( { model | flightBooker = Form.update .setReturn s model.flightBooker }
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
                | flightBooker = FlightBooker.form model.today
                , timer = Timer.init
                , isSubmitting = False
                , maybeTicket = Form.validateAsMaybe model.flightBooker
              }
            , Cmd.none
            )

        ChangedTimer subMsg ->
            ( model
            , Timer.update timerConfig subMsg model.timer
            )



-- VIEW


view : Model -> H.Html Msg
view { today, flightBooker, timer, isSubmitting, maybeTicket } =
    let
        fields =
            Form.toFields flightBooker
    in
    H.div []
        [ H.h2 [] [ H.text "Flight Booker" ]
        , H.form
            [ HA.novalidate True
            , HE.onSubmit Submit
            ]
            [ viewSelect
                { id = "flight"
                , label = "One way or return"
                , value = fields.flight
                , options =
                    [ Flight.OneWay
                    , Flight.Return
                    ]
                , toOption =
                    \flight ->
                        case flight of
                            Flight.OneWay ->
                                "one-way flight"

                            Flight.Return ->
                                "return flight"
                , fromString = Flight.fromString
                , toString = Flight.toString
                , onInput = InputFlight
                }
            , viewInput
                { id = "departure"
                , label = "Departure date (format: DD.MM.YYYY)"
                , type_ = "text"
                , field = fields.departure
                , errorToString = Date.errorToString "departure"
                , isRequired = True
                , isDisabled = False
                , attrs = []
                , onInput = InputDeparture
                }
            , viewInput
                { id = "return"
                , label = "Return date (format: DD.MM.YYYY)"
                , type_ = "text"
                , field = fields.return
                , errorToString = Date.errorToString "return"
                , isRequired = True
                , isDisabled = fields.flight == Flight.OneWay
                , attrs = []
                , onInput = InputReturn
                }
            , H.p []
                [ H.button
                    [ HA.disabled (Form.isInvalid flightBooker || isSubmitting) ]
                    [ H.text <|
                        if isSubmitting then
                            "Booking..."

                        else
                            "Book"
                    ]
                ]
            ]
        , case maybeTicket of
            Just (FlightBooker.OneWay departure) ->
                H.div []
                    [ H.h2 [] [ H.text "One-way ticket" ]
                    , H.p [] [ H.text <| "Departure: " ++ Date.toString departure ]
                    ]

            Just (FlightBooker.Return { departure, return }) ->
                H.div []
                    [ H.h2 [] [ H.text "Return ticket" ]
                    , H.p [] [ H.text <| "Departure: " ++ Date.toString departure ]
                    , H.p [] [ H.text <| "Return: " ++ Date.toString return ]
                    ]

            Nothing ->
                H.text ""
        ]


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
    , value : a
    , options : List a
    , toOption : a -> String
    , fromString : String -> Maybe a
    , toString : a -> String
    , onInput : a -> msg
    }
    -> H.Html msg
viewSelect { id, label, value, options, toOption, fromString, toString, onInput } =
    H.p []
        [ H.label [ HA.for id ] [ H.text (label ++ ": ") ]
        , H.text " "
        , H.select [ HA.id id, HA.required True, onSelect fromString onInput ] <|
            List.map
                (\option ->
                    H.option
                        [ HA.value (toString option)
                        , HA.selected (value == option)
                        ]
                        [ H.text (toOption option) ]
                )
                options
        ]


onSelect : (String -> Maybe a) -> (a -> msg) -> H.Attribute msg
onSelect fromString toMsg =
    HE.stopPropagationOn "input"
        (HE.targetValue
            |> JD.andThen
                (\s ->
                    case fromString s of
                        Just x ->
                            JD.succeed ( toMsg x, True )

                        Nothing ->
                            JD.fail <| "Unknown: " ++ s
                )
        )
