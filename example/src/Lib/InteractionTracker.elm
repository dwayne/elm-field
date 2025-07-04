module Lib.InteractionTracker exposing
    ( Messages
    , Msg
    , State
    , Status(..)
    , init
    , toMessages
    , toStatus
    , update
    )

import Lib.Task as Task


type State
    = State
        { status : Status
        }


type Status
    = NotVisited
    | Focused
    | Changed
    | Blurred


init : State
init =
    State
        { status = NotVisited
        }


type Msg msg
    = Focus
    | Input (String -> msg) String
    | Blur


update : Msg msg -> State -> ( State, Cmd msg )
update msg (State state) =
    case msg of
        Focus ->
            ( State { state | status = Focused }
            , Cmd.none
            )

        Input onInput s ->
            ( State { state | status = Changed }
            , Task.dispatch (onInput s)
            )

        Blur ->
            ( State { state | status = Blurred }
            , Cmd.none
            )


toStatus : State -> Status
toStatus (State { status }) =
    status


type alias Messages msg =
    { focus : msg
    , input : (String -> msg) -> String -> msg
    , blur : msg
    }


toMessages : (Msg msg -> msg) -> Messages msg
toMessages onChange =
    let
        onInput toMsg =
            onChange << Input toMsg
    in
    { focus = onChange Focus
    , input = onInput
    , blur = onChange Blur
    }
