module Lib.InteractiveInput exposing
    ( InputOptions
    , InteractiveInput
    , Msg
    , ViewOptions
    , init
    , modify
    , update
    , view
    )

import Html as H
import Html.Attributes as HA
import Lib.InteractionTracker as InteractionTracker


type alias InteractiveInput a =
    { field : a
    , fieldTracker : InteractionTracker.State
    }


init : a -> InteractiveInput a
init field =
    { field = field
    , fieldTracker = InteractionTracker.init
    }


modify : (a -> a) -> InteractiveInput a -> InteractiveInput a
modify f model =
    { model | field = f model.field }


type alias Msg msg =
    InteractionTracker.Msg msg


update : Msg msg -> InteractiveInput a -> ( InteractiveInput a, Cmd msg )
update msg model =
    let
        ( fieldTracker, cmd ) =
            InteractionTracker.update msg model.fieldTracker
    in
    ( { model | fieldTracker = fieldTracker }
    , cmd
    )


type alias ViewOptions a msg =
    { id : String
    , label : String
    , toInput : InputOptions a msg -> H.Html msg
    , onChange : Msg msg -> msg
    }


type alias InputOptions a msg =
    { id : String
    , field : a
    , focus : msg
    , input : (String -> msg) -> String -> msg
    , blur : msg
    }


view : ViewOptions a msg -> InteractiveInput a -> H.Html msg
view { id, label, toInput, onChange } { field, fieldTracker } =
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
            , H.span [] [ H.text (statusToString <| InteractionTracker.toStatus fieldTracker) ]
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
