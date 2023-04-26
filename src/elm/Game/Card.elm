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


getCard : PlayableCard -> Card
getCard playableCard =
    case playableCard of
        StandardCard card ->
            card

        ChoiceCard card _ ->
            card


makePlayableCard : Card -> PlayableCard
makePlayableCard card =
    case card of
        WildCard _ ->
            ChoiceCard card Color.Red

        _ ->
            StandardCard card


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


getPlayableCardColor : PlayableCard -> Maybe Color
getPlayableCardColor playableCard =
    case playableCard of
        StandardCard card ->
            getCardColor card

        ChoiceCard _ color ->
            Just color
