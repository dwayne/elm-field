module TemperatureConverter.Temperature exposing
    ( Celsius
    , Fahrenheit
    , Temperature
    , celsiusFieldType
    , errorToString
    , fahrenheitFieldType
    , fromCelsius
    , fromFahrenheit
    , high
    , low
    , toCelsius
    , toFahrenheit
    , toFloat
    , toString
    )

import Field as F exposing (Error)


type Temperature a
    = Temperature Float


type Celsius
    = Celsius


type Fahrenheit
    = Fahrenheit


fromCelsius : String -> Result Error (Temperature Celsius)
fromCelsius =
    F.float.fromString >> Result.andThen validate


fromFahrenheit : String -> Result Error (Temperature Fahrenheit)
fromFahrenheit =
    F.float.fromString >> Result.andThen validate


low : Float
low =
    -1000


high : Float
high =
    1000


validate : Float -> Result Error (Temperature a)
validate f =
    if low <= f && f <= high then
        Ok (Temperature f)

    else if f < low then
        Err (F.validationError <| "Too low: " ++ String.fromFloat f ++ ".")

    else
        Err (F.validationError <| "Too high: " ++ String.fromFloat f ++ ".")


toCelsius : Temperature Fahrenheit -> Temperature Celsius
toCelsius (Temperature f) =
    Temperature (ftoc f)


toFahrenheit : Temperature Celsius -> Temperature Fahrenheit
toFahrenheit (Temperature c) =
    Temperature (ctof c)


toFloat : Temperature a -> Float
toFloat (Temperature f) =
    f


toString : Temperature a -> String
toString (Temperature f) =
    String.fromFloat f


ftoc : Float -> Float
ftoc f =
    (f - 32) * (5 / 9)


ctof : Float -> Float
ctof c =
    c * (9 / 5) + 32



-- FIELD


celsiusFieldType : F.Type (Temperature Celsius)
celsiusFieldType =
    F.customType
        { fromString = fromCelsius
        , toString = toString
        }


fahrenheitFieldType : F.Type (Temperature Fahrenheit)
fahrenheitFieldType =
    F.customType
        { fromString = fromFahrenheit
        , toString = toString
        }


errorToString : Error -> String
errorToString =
    F.errorToString
        { onBlank = "The field is required."
        , onSyntaxError = always "The field is not a number."
        , onValidationError = identity
        }
