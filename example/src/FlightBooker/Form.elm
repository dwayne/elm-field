module FlightBooker.Form exposing
    ( Error
    , Fields
    , Form
    , Setters
    , Ticket(..)
    , form
    )

import Field as F exposing (Field)
import FlightBooker.Date as Date exposing (Date)
import FlightBooker.Flight as Flight exposing (Flight)
import Lib.Form as Form
import Validation as V exposing (Validation)



-- FORM


type alias Form =
    Form.Form Fields Setters Error Ticket


type alias Fields =
    { flight : Flight
    , departure : Field Date
    , maybeReturn : Maybe (Field Date)
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
    , maybeReturn = Nothing
    }



-- SETTERS


setters : Setters
setters =
    { setFlight =
        \{ today, flight } fields ->
            let
                maybeReturn =
                    case flight of
                        Flight.OneWay ->
                            fields.maybeReturn

                        Flight.Return ->
                            if fields.maybeReturn == Nothing then
                                case F.toMaybe fields.departure of
                                    Just departure ->
                                        Just <| F.fromValue Date.fieldType departure

                                    Nothing ->
                                        Just <| F.fromValue Date.fieldType today

                            else
                                fields.maybeReturn
            in
            { fields | flight = flight, maybeReturn = maybeReturn }
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
                    case fields.maybeReturn of
                        Nothing ->
                            newFields

                        Just return ->
                            updateReturn return departure newFields
    , setReturn =
        \s fields ->
            case fields.maybeReturn of
                Nothing ->
                    fields

                Just return ->
                    let
                        newReturn =
                            F.setFromString s return

                        newFields =
                            { fields | maybeReturn = Just newReturn }
                    in
                    updateReturn newReturn fields.departure newFields
    }


updateReturn : Field Date -> Field Date -> Fields -> Fields
updateReturn return departure fields =
    case ( F.toMaybe return, F.toMaybe departure ) of
        ( Just returnDate, Just departureDate ) ->
            if returnDate |> Date.isLaterThan departureDate then
                fields

            else
                { fields
                    | maybeReturn =
                        return
                            |> F.setCustomError "The return date must be on or after the departure date."
                            |> F.dirty
                            |> Just
                }

        _ ->
            fields



-- VALIDATE


validate : Fields -> Validation Error Ticket
validate fields =
    case fields.flight of
        Flight.OneWay ->
            fields.departure
                |> F.toValidation
                |> V.mapError DepartureError
                |> V.map OneWay

        Flight.Return ->
            case fields.maybeReturn of
                Nothing ->
                    --
                    -- This should NEVER happen.
                    --
                    V.fail (ReturnError <| F.customError "The return date is missing.")

                Just returnField ->
                    (\departure return ->
                        Return
                            { departure = departure
                            , return = return
                            }
                    )
                        |> F.get (fields.departure |> F.mapError DepartureError)
                        |> F.and (returnField |> F.mapError ReturnError)
