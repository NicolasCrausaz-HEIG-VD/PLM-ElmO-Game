module Game.Card exposing (..)

import Game.Color as Color exposing (Color)

type WildCardType
    = Standard
    | DrawFour

type Card
    = NumberCard Int Color
    | DrawCard Color
    | SkipCard Color
    | ReverseCard Color
    | WildCard WildCardType

-- Card with player choice of color
type PlayableCard
    = StandardCard Card
    | ChoiceCard Card Color

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

        WildCard wildCard ->
            case wildCard of
                Standard ->
                    "wild"

                DrawFour ->
                    "wild_draw_four"

getCardColor : Card -> Maybe Color
getCardColor card =
    case card of
        NumberCard _ color ->
            Just color

        DrawCard color ->
            Just color

        SkipCard color ->
            Just color

        ReverseCard color ->
            Just color

        _ ->
            Nothing