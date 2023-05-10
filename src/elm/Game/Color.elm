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

toInt : Color -> Int
toInt color =
    case color of
        Red ->
            0

        Blue ->
            1

        Green ->
            2

        Yellow ->
            3


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
