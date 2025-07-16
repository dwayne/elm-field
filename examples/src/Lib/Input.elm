module Lib.Input exposing
    ( ViewOptions
    , view
    )

import Field.Advanced as F exposing (Field)
import Html as H
import Html.Attributes as HA
import Html.Events as HE


type alias ViewOptions e a msg =
    { field : Field e a
    , isRequired : Bool
    , isDisabled : Bool
    , onInput : String -> msg
    , attrs : List (H.Attribute msg)
    }


view : ViewOptions e a msg -> H.Html msg
view { field, isRequired, isDisabled, onInput, attrs } =
    let
        requiredAttrs =
            if isRequired then
                [ HA.required True
                ]

            else
                []

        dataEmptyAttrs =
            if F.isEmpty field then
                [ HA.attribute "data-input-empty" ""
                ]

            else
                []

        otherAttrs =
            [ HA.value (F.toRawString field)
            , if isDisabled then
                HA.disabled True

              else
                HE.onInput onInput
            , HA.attribute "data-input-state" <|
                if F.isClean field then
                    "clean"

                else
                    "dirty"
            , HA.attribute "data-input-validity" <|
                if F.isValid field then
                    "valid"

                else
                    "invalid"
            ]
    in
    H.input (attrs ++ requiredAttrs ++ dataEmptyAttrs ++ otherAttrs) []
