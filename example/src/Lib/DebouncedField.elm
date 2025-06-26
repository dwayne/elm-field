module Lib.DebouncedField exposing
    ( Config
    , DebouncedField
    , Msg
    , ViewOptions
    , changeField
    , fromField
    , isReady
    , toField
    , trailing
    , update
    , view
    )

import Debouncer exposing (Debouncer)
import Field.Advanced as F exposing (Field)
import Html as H
import Html.Attributes as HA
import Lib.Input as Input
import Task


type DebouncedField e a
    = DebouncedField (State e a)


type alias State e a =
    { field : Field e a
    , ready : Bool
    , debouncer : Debouncer String
    }


fromField : Field e a -> DebouncedField e a
fromField field =
    DebouncedField
        { field = field
        , ready = False
        , debouncer = Debouncer.init
        }


isReady : DebouncedField e a -> Bool
isReady (DebouncedField s) =
    s.ready


changeField : (Field e a -> Field e a) -> DebouncedField e a -> DebouncedField e a
changeField transform (DebouncedField s) =
    DebouncedField { s | field = transform s.field }


toField : DebouncedField e a -> Field e a
toField (DebouncedField s) =
    s.field



-- CONFIG


type Config a msg
    = Config
        { debouncerConfig : Debouncer.Config String Msg
        , onInput : String -> msg
        , onReady : a -> msg
        , onChange : Msg -> msg
        }


trailing :
    { wait : Int
    , onInput : String -> msg
    , onReady : a -> msg
    , onChange : Msg -> msg
    }
    -> Config a msg
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


update : Config a msg -> Msg -> DebouncedField e a -> ( DebouncedField e a, Cmd msg )
update (Config c) msg ((DebouncedField s) as debouncedField) =
    case msg of
        Input raw ->
            let
                ( debouncer, cmd ) =
                    Debouncer.call c.debouncerConfig raw s.debouncer
            in
            ( DebouncedField
                { field = F.setFromString raw s.field
                , ready = False
                , debouncer = debouncer
                }
            , Cmd.batch
                [ Cmd.map c.onChange cmd
                , dispatch <| c.onInput raw
                ]
            )

        Ready raw ->
            let
                field =
                    F.setFromString raw s.field

                ( ready, cmd ) =
                    case F.toMaybe field of
                        Just value ->
                            ( True
                            , dispatch <| c.onReady value
                            )

                        Nothing ->
                            ( False
                            , Cmd.none
                            )
            in
            ( DebouncedField { s | field = field, ready = ready }
            , cmd
            )

        ChangedDebouncer debouncerMsg ->
            let
                ( debouncer, cmd ) =
                    Debouncer.update c.debouncerConfig debouncerMsg s.debouncer
            in
            ( DebouncedField { s | debouncer = debouncer }
            , Cmd.map c.onChange cmd
            )



-- VIEW


type alias ViewOptions e a msg =
    { field : DebouncedField e a
    , isRequired : Bool
    , isDisabled : Bool
    , config : Config a msg
    , attrs : List (H.Attribute msg)
    }


view : ViewOptions e a msg -> H.Html msg
view options =
    let
        state =
            case options.field of
                DebouncedField s ->
                    s

        config =
            case options.config of
                Config c ->
                    c

        attrs =
            options.attrs
                ++ (if state.ready then
                        [ HA.attribute "data-debounced-field-ready" ""
                        ]

                    else
                        []
                   )
    in
    Input.view
        { field = state.field
        , isRequired = options.isRequired
        , isDisabled = options.isDisabled
        , onInput = config.onChange << Input
        , attrs = attrs
        }


dispatch : msg -> Cmd msg
dispatch =
    Task.succeed >> Task.perform identity
