module Lib.InteractiveInput exposing
    ( InputOptions
    , ViewOptions
    , view
    )

import Html as H
import Html.Attributes as HA
import Lib.InteractionTracker as InteractionTracker


type alias ViewOptions a msg =
    { id : String
    , label : String
    , field : a
    , tracker : InteractionTracker.State
    , toInput : InputOptions a msg -> H.Html msg
    , onChange : InteractionTracker.Msg msg -> msg
    }


type alias InputOptions a msg =
    { id : String
    , field : a
    , focus : msg
    , input : (String -> msg) -> String -> msg
    , blur : msg
    }


view : ViewOptions a msg -> H.Html msg
view { id, label, field, tracker, toInput, onChange } =
    H.div []
        [ H.label [ HA.for id ] [ H.text label ]
        , H.div []
            [ let
                { focus, input, blur } =
                    InteractionTracker.toMessages onChange
              in
              toInput
                { id = id
                , field = field
                , focus = focus
                , input = input
                , blur = blur
                }
            , H.span [] [ H.text (statusToString <| InteractionTracker.toStatus tracker) ]
            ]
        ]


statusToString : InteractionTracker.Status -> String
statusToString status =
    case status of
        InteractionTracker.NotVisited ->
            "Not visited"

        InteractionTracker.Focused ->
            "Focused"

        InteractionTracker.Changed ->
            "Changed"

        InteractionTracker.Blurred ->
            "Blurred"
