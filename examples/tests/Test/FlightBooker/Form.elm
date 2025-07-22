module Test.FlightBooker.Form exposing (suite)

import Expect
import Field as F
import FlightBooker.Date as Date
import FlightBooker.Flight as Flight
import FlightBooker.Form as FlightBooker
import Lib.Form as Form
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "FlightBooker.Form"
        [ test "all the fields are correctly initialized and clean" <|
            \_ ->
                let
                    --
                    -- apply2 is reused from Test.Dillon.CheckIn.
                    --
                    apply2 f g field =
                        Expect.equal True (f field && g field)
                in
                FlightBooker.form Date.default
                    |> Form.toFields
                    |> Expect.all
                        [ .flight >> Expect.equal Flight.OneWay
                        , .departure >> apply2 F.isClean (F.toMaybe >> (==) (Just Date.default))
                        , .return >> apply2 F.isClean F.isEmpty
                        ]
        , let
            form =
                FlightBooker.form Date.default
          in
          describe "when flight is one-way"
            [ test "departure is required" <|
                \_ ->
                    form
                        |> Form.update .setDeparture ""
                        |> Form.toFields
                        |> .departure
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , test "departure must have the format: dd.mm.yyyy" <|
                \_ ->
                    form
                        |> Form.update .setDeparture "02-07-2025"
                        |> Form.toFields
                        |> .departure
                        |> F.toResult
                        |> Expect.equal (Err [ F.syntaxError "TODO deadEndsToString" ])
            , test "departure must be a valid calendar date" <|
                \_ ->
                    form
                        |> Form.update .setDeparture "29.02.2018"
                        |> Form.toFields
                        |> .departure
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "Invalid date: day 29 is out of range (1 to 28) for February (2018 is not a leap year); received (year 2018, month 2, day 29)" ])
            , test "flight and departure are valid" <|
                \_ ->
                    form
                        |> Form.update .setDeparture "22.07.2025"
                        |> Form.isValid
                        |> Expect.equal True
            ]
        , let
            today =
                Date.default

            form =
                FlightBooker.form today
                    |> Form.update .setFlight
                        { today = today
                        , flight = Flight.Return
                        }
          in
          describe "when flight is return"
            [ test "return is required" <|
                \_ ->
                    form
                        |> Form.update .setReturn ""
                        |> Form.toFields
                        |> .return
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , test "return must have the format: dd.mm.yyyy" <|
                \_ ->
                    form
                        |> Form.update .setReturn "02-07-2025"
                        |> Form.toFields
                        |> .return
                        |> F.toResult
                        |> Expect.equal (Err [ F.syntaxError "TODO deadEndsToString" ])
            , test "return must be a valid calendar date" <|
                \_ ->
                    form
                        |> Form.update .setReturn "29.02.2018"
                        |> Form.toFields
                        |> .return
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "Invalid date: day 29 is out of range (1 to 28) for February (2018 is not a leap year); received (year 2018, month 2, day 29)" ])
            , test "return must be on or after the departure: set departure then return" <|
                \_ ->
                    form
                        |> Form.update .setDeparture "29.07.2025"
                        |> Form.update .setReturn "22.07.2025"
                        |> Form.toFields
                        |> .return
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError "The return date must be on or after the departure date." ])
            , test "return must be on or after the departure: set return then departure" <|
                \_ ->
                    form
                        |> Form.update .setReturn "22.07.2025"
                        |> Form.update .setDeparture "29.07.2025"
                        |> Form.toFields
                        |> .return
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError "The return date must be on or after the departure date." ])
            , test "flight, departure, and return are valid" <|
                \_ ->
                    form
                        |> Form.update .setDeparture "22.07.2025"
                        |> Form.update .setReturn "29.07.2025"
                        |> Form.isValid
                        |> Expect.equal True
            ]
        ]
