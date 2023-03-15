module Game.Card exposing (..)

import Game.Color as Color exposing (Color)


type Card
    = NumberCard Int Color
    | DrawCard Color
    | SkipCard Color
    | ReverseCard Color
    | WildCard
    | WildDrawCard


toString : Card -> String
toString card =
    case card of
        NumberCard number color ->
            String.fromInt number ++ "_" ++ Color.toString color

        DrawCard color ->
            "draw_" ++ Color.toString color

        SkipCard color ->
            "skip_" ++ Color.toString color

        ReverseCard color ->
            "reverse_" ++ Color.toString color

        WildCard ->
            "wild"

        WildDrawCard ->
            "wild_draw_four"
