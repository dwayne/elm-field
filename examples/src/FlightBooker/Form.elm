module FlightBooker.Form exposing
    ( Error
    , Fields
    , Form
    , Setters
    , Ticket(..)
    , form
    )

import Field as F exposing (Field, Validation)
import FlightBooker.Date as Date exposing (Date)
import FlightBooker.Flight as Flight exposing (Flight)
import Lib.Form as Form



-- FORM


type alias Form =
    Form.Form Fields Setters Error Ticket


type alias Fields =
    { flight : Flight
    , departure : Field Date
    , return : Field Date
    }


type alias Setters =
    { setFlight : { today : Date, flight : Flight } -> Fields -> Fields
    , setDeparture : String -> Fields -> Fields
    , setReturn : String -> Fields -> Fields
    }


type Error
    = DepartureError F.Error
    | ReturnError F.Error


type Ticket
    = OneWay Date
    | Return
        { departure : Date
        , return : Date
        }


form : Date -> Form
form today =
    Form.new
        { init = init today
        , setters = setters
        , validate = validate
        }



-- INIT


init : Date -> Fields
init today =
    { flight = Flight.OneWay
    , departure = F.fromValue Date.fieldType today
    , return = F.empty Date.fieldType
    }



-- SETTERS


setters : Setters
setters =
    { setFlight =
        \{ today, flight } fields ->
            let
                return =
                    case flight of
                        Flight.OneWay ->
                            fields.return

                        Flight.Return ->
                            if F.isClean fields.return then
                                case F.toMaybe fields.departure of
                                    Just departure ->
                                        F.setFromValue departure fields.return

                                    Nothing ->
                                        F.setFromValue today fields.return

                            else
                                fields.return
            in
            { fields | flight = flight, return = return }
    , setDeparture =
        \s fields ->
            let
                departure =
                    F.setFromString s fields.departure

                newFields =
                    { fields | departure = departure }
            in
            case fields.flight of
                Flight.OneWay ->
                    newFields

                Flight.Return ->
                    updateReturn fields.return departure newFields
    , setReturn =
        \s fields ->
            let
                return =
                    F.setFromString s fields.return

                newFields =
                    { fields | return = return }
            in
            updateReturn return fields.departure newFields
    }


updateReturn : Field Date -> Field Date -> Fields -> Fields
updateReturn return departure fields =
    case ( F.toMaybe return, F.toMaybe departure ) of
        ( Just returnDate, Just departureDate ) ->
            if returnDate |> Date.isLaterThan departureDate then
                fields

            else
                { fields | return = F.setCustomError "The return date must be on or after the departure date." return }

        _ ->
            fields



-- VALIDATE


validate : Fields -> Validation Error Ticket
validate fields =
    case fields.flight of
        Flight.OneWay ->
            OneWay
                |> F.succeed (fields.departure |> F.mapError DepartureError)

        Flight.Return ->
            (\departure return ->
                Return
                    { departure = departure
                    , return = return
                    }
            )
                |> F.succeed (fields.departure |> F.mapError DepartureError)
                |> F.applyValidation (fields.return |> F.mapError ReturnError)
