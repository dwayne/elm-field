module FlightBooker.Flight exposing
    ( Flight(..)
    , fromString
    , toString
    )


type Flight
    = OneWay
    | Return


fromString : String -> Maybe Flight
fromString s =
    case s of
        "oneWay" ->
            Just OneWay

        "return" ->
            Just Return

        _ ->
            Nothing


toString : Flight -> String
toString flight =
    case flight of
        OneWay ->
            "oneWay"

        Return ->
            "return"
