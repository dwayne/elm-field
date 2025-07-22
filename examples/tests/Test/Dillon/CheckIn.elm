module Test.Dillon.CheckIn exposing (suite)

import Dillon.CheckIn as CheckIn
import Dillon.Date as Date
import Expect
import Field as F
import Lib.Form as Form
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Dillon.CheckIn"
        [ test "all the fields are correctly initialized and clean" <|
            \_ ->
                let
                    --
                    -- In the previous test suites I used:
                    --
                    -- - expectIsEmptyAndClean (Test.Example1.SignUp)
                    -- - expectIsCleanAndGood (Test.Dillon.SignUp)
                    --
                    -- apply2 generalizes both.
                    --
                    apply2 f g field =
                        Expect.equal True (f field && g field)
                in
                CheckIn.form
                    |> Form.toFields
                    |> Expect.all
                        [ .name >> apply2 F.isClean (F.toMaybe >> (==) (Just "dillon"))
                        , .checkIn >> apply2 F.isClean F.isEmpty
                        , .checkInTime >> apply2 F.isClean F.isEmpty
                        , .checkOut >> apply2 F.isClean F.isEmpty
                        , .subscribe >> Expect.equal True
                        ]
        , describe "name"
            [ test "it is required" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setName ""
                        |> Form.toFields
                        |> .name
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , test "it is valid" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setName "a"
                        |> Form.toFields
                        |> .name
                        |> F.isValid
                        |> Expect.equal True
            , test "it is trimmed" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setName " a "
                        |> Form.toFields
                        |> .name
                        |> F.toMaybe
                        |> Expect.equal (Just "a")
            ]
        , describe "check-in"
            [ test "it is required" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckIn
                            { today = Date.jul1st2025
                            , s = ""
                            }
                        |> Form.toFields
                        |> .checkIn
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , test "the date must be in ISO 8601 format" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckIn
                            { today = Date.jul1st2025
                            , s = "Jul 2nd, 2025"
                            }
                        |> Form.toFields
                        |> .checkIn
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "Expected a date in ISO 8601 format" ])
            , test "the date must be a valid calendar date" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckIn
                            { today = Date.jul1st2025
                            , s = "2018-02-29"
                            }
                        |> Form.toFields
                        |> .checkIn
                        |> F.toResult
                        --
                        -- This is one of the areas where the justinmimbs/date library
                        -- could be improved. It should not use String for errors.
                        -- Rather, it should leave it up to the consumer to decide on
                        -- the way they want to describe the error to their users.
                        --
                        |> Expect.equal (Err [ F.validationError "Invalid date: day 29 is out of range (1 to 28) for February (2018 is not a leap year); received (year 2018, month 2, day 29)" ])
            , test "you must check-in after today" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckIn
                            { today = Date.jul1st2025
                            , s = "2025-07-01"
                            }
                        |> Form.toFields
                        |> .checkIn
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError "You must check-in after today." ])
            , test "it is valid" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckIn
                            { today = Date.jul1st2025
                            , s = "2025-07-02"
                            }
                        |> Form.toFields
                        |> .checkIn
                        |> F.isValid
                        |> Expect.equal True
            , test "it is trimmed" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckIn
                            { today = Date.jul1st2025
                            , s = " 2025-07-02 "
                            }
                        |> Form.toFields
                        |> .checkIn
                        |> F.isValid
                        |> Expect.equal True
            ]
        , describe "check-in time"
            [ test "it is required" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckInTime ""
                        |> Form.toFields
                        |> .checkInTime
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , test "the time must be in the format hh:mm" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckInTime "hhmm"
                        |> Form.toFields
                        |> .checkInTime
                        |> F.toResult
                        |> Expect.equal (Err [ F.syntaxError "hhmm" ])
            , test "the time must be valid" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckInTime "05:80"
                        |> Form.toFields
                        |> .checkInTime
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "05:80" ])
            , test "you must check-in within a specific time range" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckInTime "05:30"
                        |> Form.toFields
                        |> .checkInTime
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError "You must check-in between the hours of 9am to 5pm." ])
            , test "it is valid" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckInTime "09:00"
                        |> Form.toFields
                        |> .checkInTime
                        |> F.isValid
                        |> Expect.equal True
            , test "it is trimmed" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckInTime " 17:00 "
                        |> Form.toFields
                        |> .checkInTime
                        |> F.isValid
                        |> Expect.equal True
            ]
        , describe "check out"
            [ test "it is required" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckOut ""
                        |> Form.toFields
                        |> .checkOut
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , test "the date must be in ISO 8601 format" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckOut "Jul 2nd, 2025"
                        |> Form.toFields
                        |> .checkOut
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "Expected a date in ISO 8601 format" ])
            , test "the date must be a valid calendar date" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckOut "2018-02-29"
                        |> Form.toFields
                        |> .checkOut
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "Invalid date: day 29 is out of range (1 to 28) for February (2018 is not a leap year); received (year 2018, month 2, day 29)" ])
            , test "you must check out after checking in: set check-in then check out" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckIn
                            { today = Date.jul1st2025
                            , s = "2025-07-22"
                            }
                        |> Form.update .setCheckOut "2025-07-21"
                        |> Form.toFields
                        |> .checkOut
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError "You must check out after you've checked in." ])
            , test "you must check out after checking in: set check out then check-in" <|
                --
                -- N.B. Initially this test didn't pass and it prompted me to update the setCheckIn logic.
                --
                -- The UI didn't allow this scenario to happen but the logic didn't prevent it. This
                -- presents a good case for unit testing your forms.
                --
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckOut "2025-07-21"
                        |> Form.update .setCheckIn
                            { today = Date.jul1st2025
                            , s = "2025-07-22"
                            }
                        |> Form.toFields
                        |> .checkOut
                        |> F.toResult
                        |> Expect.equal (Err [ F.customError "You must check out after you've checked in." ])
            , test "it is valid" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckOut "2025-07-21"
                        |> Form.toFields
                        |> .checkOut
                        |> F.isValid
                        |> Expect.equal True
            , test "it is trimmed" <|
                \_ ->
                    CheckIn.form
                        |> Form.update .setCheckOut " 2025-07-21 "
                        |> Form.toFields
                        |> .checkOut
                        |> F.isValid
                        |> Expect.equal True
            ]
        ]
