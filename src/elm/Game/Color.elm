module Game.Color exposing (..)


type Color
    = Red
    | Blue
    | Green
    | Yellow


toString : Color -> String
toString color =
    case color of
        Red ->
            "red"

        Blue ->
            "blue"

        Green ->
            "green"

        Yellow ->
            "yellow"


fromString : String -> Maybe Color
fromString string =
    case string of
        "red" ->
            Just Red

        "blue" ->
            Just Blue

        "green" ->
            Just Green

        "yellow" ->
            Just Yellow

        _ ->
            Nothing
