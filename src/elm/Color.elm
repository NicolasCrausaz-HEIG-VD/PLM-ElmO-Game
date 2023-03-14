module Color exposing (..)


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
