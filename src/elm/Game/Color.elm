module Game.Color exposing (..)

import Json.Decode as D
import Json.Encode as E
import Utils exposing (decodeMaybe)

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


encodeColor : Color -> E.Value
encodeColor color =
    E.string (toString color)


decodeColor : D.Decoder Color
decodeColor =
    decodeMaybe D.string fromString