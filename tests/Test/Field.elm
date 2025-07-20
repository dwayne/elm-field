module Test.Field exposing (suite)

import Expect
import Field as F exposing (defaultCustomBoolOptions)
import Fuzz exposing (Fuzzer)
import Set
import Test exposing (Test, describe, fuzz, test)
import Test.Fixtures.Username as Username
import Test.Lib.Fuzz as Fuzz


suite : Test
suite =
    describe "Field"
        [ primitiveTypesSuite
        , userDefinedTypesSuite
        ]


primitiveTypesSuite : Test
primitiveTypesSuite =
    describe "Primitive Types"
        [ intSuite
        , floatSuite
        , boolSuite
        , charSuite
        , stringSuite
        ]


intSuite : Test
intSuite =
    describe "Int"
        [ describe "int"
            [ fuzz Fuzz.int "integer strings in the range Random.minInt to Random.maxInt inclusive" <|
                \n ->
                    F.fromString F.int (String.fromInt n)
                        |> F.toMaybe
                        |> Expect.equal (Just n)
            , test "integer string surrounded by whitespace" <|
                \_ ->
                    F.fromString F.int "  5 \t "
                        |> F.toMaybe
                        |> Expect.equal (Just 5)
            , fuzz Fuzz.int "integers in the range Random.minInt to Random.maxInt inclusive" <|
                \n ->
                    F.fromValue F.int n
                        |> F.toMaybe
                        |> Expect.equal (Just n)
            , fuzz Fuzz.blankString "blank strings" <|
                \s ->
                    F.fromString F.int s
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , test "non-integer string" <|
                \_ ->
                    F.fromString F.int "one"
                        |> F.toResult
                        |> Expect.equal (Err [ F.syntaxError "one" ])
            ]
        , describe "nonNegativeInt"
            [ test "zero" <|
                \_ ->
                    F.fromValue F.nonNegativeInt 0
                        |> F.toMaybe
                        |> Expect.equal (Just 0)
            , test "positive integer" <|
                \_ ->
                    F.fromValue F.nonNegativeInt 1
                        |> F.toMaybe
                        |> Expect.equal (Just 1)
            , test "negative integer" <|
                \_ ->
                    F.fromValue F.nonNegativeInt -1
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "-1" ])
            ]
        , describe "positiveInt"
            [ test "zero" <|
                \_ ->
                    F.fromValue F.positiveInt 0
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "0" ])
            , test "positive integer" <|
                \_ ->
                    F.fromValue F.positiveInt 1
                        |> F.toMaybe
                        |> Expect.equal (Just 1)
            , test "negative integer" <|
                \_ ->
                    F.fromValue F.positiveInt -1
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "-1" ])
            ]
        , describe "nonPositiveInt"
            [ test "zero" <|
                \_ ->
                    F.fromValue F.nonPositiveInt 0
                        |> F.toMaybe
                        |> Expect.equal (Just 0)
            , test "positive integer" <|
                \_ ->
                    F.fromValue F.nonPositiveInt 1
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "1" ])
            , test "negative integer" <|
                \_ ->
                    F.fromValue F.nonPositiveInt -1
                        |> F.toMaybe
                        |> Expect.equal (Just -1)
            ]
        , describe "negativeInt"
            [ test "zero" <|
                \_ ->
                    F.fromValue F.negativeInt 0
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "0" ])
            , test "positive integer" <|
                \_ ->
                    F.fromValue F.negativeInt 1
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "1" ])
            , test "negative integer" <|
                \_ ->
                    F.fromValue F.negativeInt -1
                        |> F.toMaybe
                        |> Expect.equal (Just -1)
            ]
        ]


floatSuite : Test
floatSuite =
    describe "Float"
        [ describe "float"
            [ fuzz Fuzz.niceFloat "float strings" <|
                \f ->
                    F.fromString F.float (String.fromFloat f)
                        |> F.toMaybe
                        |> Expect.equal (Just f)
            , test "float string surrounded by whitespace" <|
                \_ ->
                    F.fromString F.float "  3.14 \t "
                        |> F.toMaybe
                        |> Expect.equal (Just 3.14)
            , fuzz Fuzz.niceFloat "floats" <|
                \f ->
                    F.fromValue F.float f
                        |> F.toMaybe
                        |> Expect.equal (Just f)
            , fuzz Fuzz.blankString "blank strings" <|
                \s ->
                    F.fromString F.float s
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , test "non-float string" <|
                \_ ->
                    F.fromString F.int "pi"
                        |> F.toResult
                        |> Expect.equal (Err [ F.syntaxError "pi" ])
            ]
        , describe "nonNegativeFloat"
            [ test "zero" <|
                \_ ->
                    F.fromValue F.nonNegativeFloat 0
                        |> F.toMaybe
                        |> Expect.equal (Just 0)
            , test "positive float" <|
                \_ ->
                    F.fromValue F.nonNegativeFloat 0.1
                        |> F.toMaybe
                        |> Expect.equal (Just 0.1)
            , test "negative float" <|
                \_ ->
                    F.fromValue F.nonNegativeFloat -0.1
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "-0.1" ])
            ]
        , describe "positiveFloat"
            [ test "zero" <|
                \_ ->
                    F.fromValue F.positiveFloat 0
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "0" ])
            , test "positive float" <|
                \_ ->
                    F.fromValue F.positiveFloat 0.1
                        |> F.toMaybe
                        |> Expect.equal (Just 0.1)
            , test "negative float" <|
                \_ ->
                    F.fromValue F.positiveFloat -0.1
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "-0.1" ])
            ]
        , describe "nonPositiveFloat"
            [ test "zero" <|
                \_ ->
                    F.fromValue F.nonPositiveFloat 0
                        |> F.toMaybe
                        |> Expect.equal (Just 0)
            , test "positive float" <|
                \_ ->
                    F.fromValue F.nonPositiveFloat 0.1
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "0.1" ])
            , test "negative float" <|
                \_ ->
                    F.fromValue F.nonPositiveFloat -0.1
                        |> F.toMaybe
                        |> Expect.equal (Just -0.1)
            ]
        , describe "negativeFloat"
            [ test "zero" <|
                \_ ->
                    F.fromValue F.negativeFloat 0
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "0" ])
            , test "positive float" <|
                \_ ->
                    F.fromValue F.negativeFloat 0.1
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "0.1" ])
            , test "negative float" <|
                \_ ->
                    F.fromValue F.negativeFloat -0.1
                        |> F.toMaybe
                        |> Expect.equal (Just -0.1)
            ]
        ]


boolSuite : Test
boolSuite =
    describe "Bool"
        [ describe "defaultTruthy"
            [ test "the default true values" <|
                \_ ->
                    F.defaultTruthy
                        |> Expect.equal
                            (Set.fromList
                                [ "true"
                                , "1"
                                , "yes"
                                , "on"
                                , "y"
                                , "enabled"
                                ]
                            )
            ]
        , describe "defaultFalsy"
            [ test "the default false values" <|
                \_ ->
                    F.defaultFalsy
                        |> Expect.equal
                            (Set.fromList
                                [ "false"
                                , "0"
                                , "no"
                                , "off"
                                , "n"
                                , "disabled"
                                ]
                            )
            ]
        , describe "defaultBoolToString"
            [ test "it returns true for True" <|
                \_ ->
                    F.defaultBoolToString True
                        |> Expect.equal "true"
            , test "it returns false for False" <|
                \_ ->
                    F.defaultBoolToString False
                        |> Expect.equal "false"
            ]
        , describe "bool"
            [ fuzz (Fuzz.oneOfValues <| Set.toList F.defaultTruthy) "true strings" <|
                \s ->
                    F.fromString F.bool s
                        |> F.toMaybe
                        |> Expect.equal (Just True)
            , fuzz (Fuzz.oneOfValues <| Set.toList F.defaultFalsy) "false strings" <|
                \s ->
                    F.fromString F.bool s
                        |> F.toMaybe
                        |> Expect.equal (Just False)
            , test "bool string surrounded by whitespace" <|
                \_ ->
                    F.fromString F.bool "  true \t "
                        |> F.toMaybe
                        |> Expect.equal (Just True)
            , fuzz (Fuzz.oneOfValues [ ( "TrUe", True ), ( "FaLsE", False ), ( "yeS", True ), ( "NO", False ) ]) "case insensitivity" <|
                \( s, b ) ->
                    F.fromString F.bool s
                        |> F.toMaybe
                        |> Expect.equal (Just b)
            , let
                caseSensitiveBool =
                    F.customSubsetOfBool { defaultCustomBoolOptions | caseSensitive = True } (always True)
              in
              fuzz (Fuzz.oneOfValues [ "TrUe", "FaLsE", "yeS", "NO" ]) "case sensitivity" <|
                \s ->
                    F.fromString caseSensitiveBool s
                        |> F.toResult
                        |> Expect.equal (Err [ F.syntaxError s ])
            , fuzz Fuzz.blankString "blank strings" <|
                \s ->
                    F.fromString F.bool s
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , fuzz (Fuzz.oneOfValues [ "t", "#t", "f", "#f" ]) "invalid strings" <|
                \s ->
                    F.fromString F.bool s
                        |> F.toResult
                        |> Expect.equal (Err [ F.syntaxError s ])
            , let
                customTrueFalseBool =
                    F.customSubsetOfBool
                        { defaultCustomBoolOptions
                            | truthy = Set.fromList [ "t", "#t" ]
                            , falsy = Set.fromList [ "f", "#f" ]
                        }
                        (always True)
              in
              fuzz (Fuzz.oneOfValues [ ( "t", True ), ( "#t", True ), ( "f", False ), ( "#f", False ) ]) "custom true and false strings" <|
                \( s, b ) ->
                    F.fromString customTrueFalseBool s
                        |> F.toMaybe
                        |> Expect.equal (Just b)
            ]
        , describe "true"
            [ fuzz (Fuzz.oneOfValues <| Set.toList F.defaultTruthy) "true strings are valid" <|
                \s ->
                    F.fromString F.true s
                        |> F.toMaybe
                        |> Expect.equal (Just True)
            , fuzz (Fuzz.oneOfValues <| Set.toList F.defaultFalsy) "falsy strings are invalid" <|
                \s ->
                    F.fromString F.true s
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "false" ])
            ]
        , describe "false"
            [ fuzz (Fuzz.oneOfValues <| Set.toList F.defaultTruthy) "truthy strings are invalid" <|
                \s ->
                    F.fromString F.false s
                        |> F.toResult
                        |> Expect.equal (Err [ F.validationError "true" ])
            , fuzz (Fuzz.oneOfValues <| Set.toList F.defaultFalsy) "falsy strings are valid" <|
                \s ->
                    F.fromString F.false s
                        |> F.toMaybe
                        |> Expect.equal (Just False)
            ]
        ]


charSuite : Test
charSuite =
    describe "Char"
        [ describe "char"
            [ fuzz Fuzz.char "strings of exactly one character" <|
                \c ->
                    F.fromString F.char (String.fromChar c)
                        |> F.toMaybe
                        |> Expect.equal (Just c)
            , fuzz Fuzz.nonSingleCharString "strings of zero, two, or more characters" <|
                \s ->
                    F.fromString F.char s
                        |> F.toResult
                        |> Expect.equal
                            (Err
                                [ if String.isEmpty s then
                                    F.blankError

                                  else
                                    F.syntaxError s
                                ]
                            )
            ]
        , describe "subsetOfChar"
            [ describe "digits"
                [ fuzz (Fuzz.oneOfValues [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]) "digit" <|
                    \c ->
                        F.fromString (F.subsetOfChar Char.isDigit) (String.fromChar c)
                            |> F.toMaybe
                            |> Expect.equal (Just c)
                , fuzz (Fuzz.oneOfValues [ '~', '@', 'a', 'A', 'f', ':', ',', ' ', '?' ]) "non-digit" <|
                    \c ->
                        let
                            s =
                                String.fromChar c
                        in
                        F.fromString (F.subsetOfChar Char.isDigit) s
                            |> F.toResult
                            |> Expect.equal (Err [ F.validationError s ])
                ]
            ]
        ]


stringSuite : Test
stringSuite =
    describe "String"
        [ describe "string"
            [ fuzz Fuzz.string "all strings are accepted and trimmed" <|
                \s ->
                    F.fromString F.string s
                        |> F.toMaybe
                        |> Expect.equal (Just <| String.trim s)
            ]
        , describe "subsetOfString"
            [ describe "type DELETE to confirm removal of the resource"
                [ test "DELETE" <|
                    \_ ->
                        F.fromString (F.subsetOfString ((==) "DELETE")) "DELETE"
                            |> F.toMaybe
                            |> Expect.equal (Just "DELETE")
                , test "delete" <|
                    \_ ->
                        F.fromString (F.subsetOfString ((==) "DELETE")) "delete"
                            |> F.toResult
                            |> Expect.equal (Err [ F.validationError "delete" ])
                , test "the empty string" <|
                    \_ ->
                        F.fromString (F.subsetOfString ((==) "DELETE")) ""
                            |> F.toResult
                            |> Expect.equal (Err [ F.validationError "" ])
                , test "a non-empty blank string" <|
                    \_ ->
                        F.fromString (F.subsetOfString ((==) "DELETE")) " \t "
                            |> F.toResult
                            |> Expect.equal (Err [ F.validationError " \t " ])
                ]
            ]
        , describe "nonEmptyString"
            [ fuzz (Fuzz.oneOfValues [ " ", "\t", " \t  \t   \t    ", "Hello, world!", " Hi! " ]) "non-empty strings" <|
                \s ->
                    F.fromString F.nonEmptyString s
                        |> F.toMaybe
                        |> Expect.equal (Just s)
            , test "the empty string" <|
                \_ ->
                    F.fromString F.nonEmptyString ""
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            ]
        , describe "subsetOfNonEmptyString"
            [ describe "type DELETE to confirm removal of the resource"
                [ test "DELETE" <|
                    \_ ->
                        F.fromString (F.subsetOfNonEmptyString ((==) "DELETE")) "DELETE"
                            |> F.toMaybe
                            |> Expect.equal (Just "DELETE")
                , test "delete" <|
                    \_ ->
                        F.fromString (F.subsetOfNonEmptyString ((==) "DELETE")) "delete"
                            |> F.toResult
                            |> Expect.equal (Err [ F.validationError "delete" ])
                , test "the empty string" <|
                    \_ ->
                        F.fromString (F.subsetOfNonEmptyString ((==) "DELETE")) ""
                            |> F.toResult
                            |> Expect.equal (Err [ F.blankError ])
                , test "a non-empty blank string" <|
                    \_ ->
                        F.fromString (F.subsetOfNonEmptyString ((==) "DELETE")) " \t "
                            |> F.toResult
                            |> Expect.equal (Err [ F.validationError " \t " ])
                ]
            ]
        , describe "nonBlankString"
            [ fuzz Fuzz.string "non-blank strings are accepted and trimmed" <|
                \s ->
                    if String.isEmpty (String.trim s) then
                        F.fromString F.nonBlankString s
                            |> F.toResult
                            |> Expect.equal (Err [ F.blankError ])

                    else
                        F.fromString F.nonBlankString s
                            |> F.toMaybe
                            |> Expect.equal (Just <| String.trim s)
            ]
        , describe "subsetOfNonBlankString"
            [ describe "type DELETE to confirm removal of the resource"
                [ test "DELETE" <|
                    \_ ->
                        F.fromString (F.subsetOfNonBlankString ((==) "DELETE")) "DELETE"
                            |> F.toMaybe
                            |> Expect.equal (Just "DELETE")
                , test "delete" <|
                    \_ ->
                        F.fromString (F.subsetOfNonBlankString ((==) "DELETE")) "delete"
                            |> F.toResult
                            |> Expect.equal (Err [ F.validationError "delete" ])
                , test "the empty string" <|
                    \_ ->
                        F.fromString (F.subsetOfNonBlankString ((==) "DELETE")) ""
                            |> F.toResult
                            |> Expect.equal (Err [ F.blankError ])
                , test "a non-empty blank string" <|
                    \_ ->
                        F.fromString (F.subsetOfNonBlankString ((==) "DELETE")) " \t "
                            |> F.toResult
                            |> Expect.equal (Err [ F.blankError ])
                ]
            ]
        ]


userDefinedTypesSuite : Test
userDefinedTypesSuite =
    describe "User-defined"
        [ describe "Example: Username"
            [ test "it is required" <|
                \_ ->
                    F.fromString Username.fieldType ""
                        |> F.toResult
                        |> Expect.equal (Err [ F.blankError ])
            , fuzz (Fuzz.asciiStringOfLengthBetween 1 4) "it must be at least 5 characters long" <|
                \s ->
                    F.fromString Username.fieldType s
                        |> F.toResult
                        |> Expect.equal
                            (Err
                                [ if String.isEmpty (String.trim s) then
                                    F.blankError

                                  else
                                    F.validationError "Too short"
                                ]
                            )
            , fuzz (Fuzz.oneOfValues [ "abcde", "abcdef", "abcdefg" ]) "valid usernames" <|
                \s ->
                    F.fromString Username.fieldType s
                        |> F.toResult
                        |> Result.map Username.toString
                        |> Expect.equal (Ok s)
            ]
        ]
