module TemperatureConverter.Main exposing (main)

import Browser as B
import Field as F exposing (Field)
import Html as H
import Lib.Input as Input
import TemperatureConverter.Temperature as Temperature exposing (Celsius, Fahrenheit, Temperature)


main : Program () Model Msg
main =
    B.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { celsius : Field (Temperature Celsius)
    , fahrenheit : Field (Temperature Fahrenheit)
    }


init : Model
init =
    { celsius = F.fromString Temperature.celsiusFieldType ""
    , fahrenheit = F.fromString Temperature.fahrenheitFieldType ""
    }



-- UPDATE


type Msg
    = InputCelsius String
    | InputFahrenheit String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputCelsius s ->
            let
                celsius =
                    F.setFromString s model.celsius

                fahrenheit =
                    case F.toMaybe celsius of
                        Just c ->
                            F.setFromValue (Temperature.toFahrenheit c) model.fahrenheit

                        Nothing ->
                            model.fahrenheit
            in
            { model | celsius = celsius, fahrenheit = fahrenheit }

        InputFahrenheit s ->
            let
                fahrenheit =
                    F.setFromString s model.fahrenheit

                celsius =
                    case F.toMaybe fahrenheit of
                        Just f ->
                            F.setFromValue (Temperature.toCelsius f) model.celsius

                        Nothing ->
                            model.celsius
            in
            { model | celsius = celsius, fahrenheit = fahrenheit }



-- VIEW


view : Model -> H.Html Msg
view { celsius, fahrenheit } =
    H.div []
        [ H.h1 [] [ H.text "Temperature Converter" ]
        , H.p []
            [ Input.view
                { field = celsius
                , isRequired = True
                , isDisabled = False
                , onInput = InputCelsius
                , attrs = []
                }
            ]
        , H.p []
            [ Input.view
                { field = fahrenheit
                , isRequired = True
                , isDisabled = False
                , onInput = InputFahrenheit
                , attrs = []
                }
            ]
        ]
