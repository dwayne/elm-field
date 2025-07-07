module Lib.InteractiveInput exposing
    ( InputOptions
    , ViewOptions
    , view
    )

import Field as F exposing (Error, Field)
import Html as H
import Html.Attributes as HA
import Lib.InteractionTracker as InteractionTracker


type alias ViewOptions a msg =
    { id : String
    , label : String
    , field : Field a
    , tracker : InteractionTracker.State
    , errorToString : Error -> String
    , toInput : InputOptions a msg -> H.Html msg
    , onChange : InteractionTracker.Msg msg -> msg
    }


type alias InputOptions a msg =
    { id : String
    , field : Field a
    , focus : msg
    , input : (String -> msg) -> String -> msg
    , blur : msg
    }


view : ViewOptions a msg -> H.Html msg
view { id, label, field, tracker, errorToString, toInput, onChange } =
    let
        status =
            InteractionTracker.toStatus tracker

        { focus, input, blur } =
            InteractionTracker.toMessages onChange
    in
    H.p []
        [ H.label [ HA.for id ] [ H.text (label ++ ": ") ]
        , H.text " "
        , toInput
            { id = id
            , field = field
            , focus = focus
            , input = input
            , blur = blur
            }
        , H.text " "
        , H.span [] [ H.text <| statusToString status ]
        , if status == InteractionTracker.Blurred then
            field
                |> F.allErrors
                |> List.map
                    (\e ->
                        H.li [ HA.style "color" "red" ] [ H.text <| errorToString e ]
                    )
                |> H.ul []

          else
            H.text ""
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
