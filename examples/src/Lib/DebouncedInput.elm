module Lib.DebouncedInput exposing
    ( Config
    , DebouncedInput
    , Msg
    , ViewOptions
    , init
    , isReady
    , trailing
    , update
    , view
    )

import Debouncer exposing (Debouncer)
import Field.Advanced exposing (Field)
import Html as H
import Html.Attributes as HA
import Lib.Input as Input
import Task


type DebouncedInput
    = DebouncedInput State


type alias State =
    { debouncer : Debouncer String
    , ready : Bool
    }


init : DebouncedInput
init =
    DebouncedInput
        { debouncer = Debouncer.init
        , ready = False
        }


isReady : DebouncedInput -> Bool
isReady (DebouncedInput { ready }) =
    ready



-- CONFIG


type Config msg
    = Config
        { debouncerConfig : Debouncer.Config String Msg
        , onInput : String -> msg
        , onReady : String -> msg
        , onChange : Msg -> msg
        }


trailing :
    { wait : Int
    , onInput : String -> msg
    , onReady : String -> msg
    , onChange : Msg -> msg
    }
    -> Config msg
trailing { wait, onInput, onReady, onChange } =
    Config
        { debouncerConfig =
            Debouncer.trailing
                { wait = wait
                , onReady = Ready
                , onChange = ChangedDebouncer
                }
        , onInput = onInput
        , onReady = onReady
        , onChange = onChange
        }



-- UPDATE


type Msg
    = Input String
    | Ready String
    | ChangedDebouncer Debouncer.Msg


update : Config msg -> Msg -> DebouncedInput -> ( DebouncedInput, Cmd msg )
update (Config c) msg (DebouncedInput s) =
    case msg of
        Input raw ->
            let
                ( debouncer, cmd ) =
                    Debouncer.call c.debouncerConfig raw s.debouncer
            in
            ( DebouncedInput
                { ready = False
                , debouncer = debouncer
                }
            , Cmd.batch
                [ Cmd.map c.onChange cmd
                , dispatch <| c.onInput raw
                ]
            )

        Ready raw ->
            ( DebouncedInput { s | ready = True }
            , dispatch <| c.onReady raw
            )

        ChangedDebouncer debouncerMsg ->
            let
                ( debouncer, cmd ) =
                    Debouncer.update c.debouncerConfig debouncerMsg s.debouncer
            in
            ( DebouncedInput { s | debouncer = debouncer }
            , Cmd.map c.onChange cmd
            )



-- VIEW


type alias ViewOptions e a msg =
    { field : Field e a
    , debouncedInput : DebouncedInput
    , isRequired : Bool
    , isDisabled : Bool
    , config : Config msg
    , attrs : List (H.Attribute msg)
    }


view : ViewOptions e a msg -> H.Html msg
view options =
    let
        config =
            case options.config of
                Config c ->
                    c

        attrs =
            options.attrs
                ++ (if isReady options.debouncedInput then
                        [ HA.attribute "data-debounced-input-ready" ""
                        ]

                    else
                        []
                   )
    in
    Input.view
        { field = options.field
        , isRequired = options.isRequired
        , isDisabled = options.isDisabled
        , onInput = config.onChange << Input
        , attrs = attrs
        }


dispatch : msg -> Cmd msg
dispatch =
    Task.succeed >> Task.perform identity
