module Lib.Select exposing
    ( ViewOptions
    , view
    )

import Field.Advanced as F exposing (Field)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Lib.Selection.NonEmpty as Selection exposing (Selection)



--
-- Lib.Selection -- at most one can be selected (zero or one)
-- Lib.Selection.Multi -- zero or more can be selected
-- Lib.Selection.NonEmpty -- exactly one can be selected
-- Lib.Selection.NonEmpty.Multi -- one or more can be selected
--


type alias ViewOptions e a msg =
    { field : Field e a
    , options : Selection a -- TODO: Implement a selection list.
    , toOption : a -> String
    , errorToString : e -> String
    , isRequired : Bool
    , isDisabled : Bool
    , onInput : a -> msg
    , attrs : List (H.Attribute msg)
    , optionAttrs : a -> List (H.Attribute msg)
    }


view : ViewOptions e a msg -> H.Html msg
view { field, options, toOption, errorToString, isRequired, isDisabled, onInput, attrs, optionAttrs } =
    let
        --
        -- TODO: Notice the repetition between this view and Input.view.
        --
        -- Reflect on what changes and what remains the same.
        --
        requiredAttrs =
            if isRequired then
                [ HA.required True
                ]

            else
                []

        dataEmptyAttrs =
            if F.isEmpty field then
                [ HA.attribute "data-select-empty" ""
                ]

            else
                []

        otherAttrs =
            [ if isDisabled then
                HA.disabled True

              else
                onSelect field errorToString onInput
            , HA.attribute "data-select-state" <|
                if F.isClean field then
                    "clean"

                else
                    "dirty"
            , HA.attribute "data-select-validity" <|
                if F.isValid field then
                    "valid"

                else
                    "invalid"
            ]

        { toString } =
            F.toType field
    in
    H.select
        (attrs ++ requiredAttrs ++ dataEmptyAttrs ++ otherAttrs)
        (Selection.toList
            { onSelected =
                \x ->
                    H.option
                        (optionAttrs x
                            ++ [ HA.value (toString x)
                               , HA.selected True
                               ]
                        )
                        [ H.text (toOption x) ]
            , onOther =
                \x ->
                    H.option
                        (optionAttrs x
                            ++ [ HA.value (toString x)
                               ]
                        )
                        [ H.text (toOption x) ]
            }
            options
        )


onSelect : Field e a -> (e -> String) -> (a -> msg) -> H.Attribute msg
onSelect field errorToString toMsg =
    let
        { fromString } =
            F.toType field
    in
    HE.stopPropagationOn "input"
        (HE.targetValue
            |> JD.andThen
                (\s ->
                    case fromString s of
                        Ok x ->
                            JD.succeed ( toMsg x, True )

                        Err err ->
                            JD.fail (errorToString err)
                )
        )
