module Dillon.CheckIn exposing
    ( CheckIn
    , Error
    , Fields
    , Output
    , Setters
    , form
    , nameErrorToString
    )

import Dillon.Date as Date exposing (Date)
import Dillon.Time as Time exposing (Time)
import Field as F exposing (Field)
import Lib.Form as Form exposing (Form)
import Validation exposing (Validation)



-- FORM


type alias CheckIn =
    Form Fields Setters Error Output


type alias Fields =
    { name : Field String
    , checkIn : Field Date
    , checkInTime : Field Time
    , checkOut : Field Date
    , subscribe : Bool
    }


type alias Setters =
    { setName : String -> Fields -> Fields
    , setCheckIn : { today : Date, s : String } -> Fields -> Fields
    , setCheckInTime : String -> Fields -> Fields
    , setCheckOut : String -> Fields -> Fields
    , setSubscribe : Bool -> Fields -> Fields
    }


type Error
    = NameError F.Error
    | CheckInError F.Error
    | CheckInTimeError F.Error
    | CheckOutError F.Error


type alias Output =
    { name : String
    , stay : Stay
    , isSubscribed : Bool
    }


type alias Stay =
    { date : Date
    , time : Time
    , nights : Int
    }


form : CheckIn
form =
    Form.new
        { init = init
        , setters = setters
        , validate = validate
        }



-- INIT


init : Fields
init =
    { name = F.fromString F.nonBlankString "dillon"
    , checkIn = F.empty Date.fieldType
    , checkInTime = F.empty Time.fieldType
    , checkOut = F.empty Date.fieldType
    , subscribe = True
    }



-- SETTERS


setters : Setters
setters =
    { setName =
        \s fields ->
            { fields | name = F.setFromString s fields.name }
    , setCheckIn =
        \{ today, s } fields ->
            --
            -- N.B. You can pass in whatever data you need from the outside via a tuple or record.
            --
            let
                checkIn =
                    F.setFromString s fields.checkIn
            in
            case F.toMaybe checkIn of
                Just date ->
                    if date |> Date.isAfter today then
                        { fields | checkIn = checkIn }

                    else
                        { fields | checkIn = F.setCustomError "You must check in after today." checkIn }

                Nothing ->
                    { fields | checkIn = checkIn }
    , setCheckInTime =
        \s fields ->
            let
                checkInTime =
                    F.setFromString s fields.checkInTime
            in
            case F.toMaybe checkInTime of
                Just time ->
                    if time |> Time.isBetween Time.nineAm Time.fivePm then
                        { fields | checkInTime = checkInTime }

                    else
                        { fields | checkInTime = F.setCustomError "You must check in between the hours of 9am to 5pm." checkInTime }

                Nothing ->
                    { fields | checkInTime = checkInTime }
    , setCheckOut =
        \s fields ->
            let
                checkOut =
                    F.setFromString s fields.checkOut

                maybeCheckOutIsAfterCheckIn =
                    Date.isAfter
                        |> F.get fields.checkIn
                        |> F.and checkOut
                        |> F.andMaybe
            in
            case maybeCheckOutIsAfterCheckIn of
                Just checkOutIsAfterCheckIn ->
                    if checkOutIsAfterCheckIn then
                        { fields | checkOut = checkOut }

                    else
                        { fields | checkOut = F.setCustomError "You must check out after you've checked in." checkOut }

                Nothing ->
                    { fields | checkOut = checkOut }
    , setSubscribe =
        \b fields ->
            { fields | subscribe = b }
    }



-- VALIDATE


validate : Fields -> Validation Error Output
validate fields =
    (\name checkIn checkInTime checkOut ->
        Output
            name
            (Stay checkIn checkInTime (Date.nights checkIn checkOut))
            fields.subscribe
    )
        |> F.get (fields.name |> F.mapError NameError)
        |> F.and (fields.checkIn |> F.mapError CheckInError)
        |> F.and (fields.checkInTime |> F.mapError CheckInTimeError)
        |> F.and (fields.checkOut |> F.mapError CheckOutError)



-- NAME


nameErrorToString : F.Error -> String
nameErrorToString =
    F.errorToString
        { onBlank = "The name is required."
        , onSyntaxError = always ""
        , onValidationError = always ""
        }
