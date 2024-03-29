module Game.Card exposing (..)

import Game.Color as Color exposing (Color)
import Json.Decode as D
import Json.Encode as E
import Utils exposing (decodeMaybe)


type WildCardType
    = Standard
    | DrawFour


type Card
    = NumberCard Int Color
    | DrawCard Color
    | SkipCard Color
    | ReverseCard Color
    | WildCard WildCardType


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


fromString : String -> Maybe Card
fromString string =
    let
        parts =
            String.split "_" string
    in
    case parts of
        [ "draw", colorStr ] ->
            Color.fromString colorStr
                |> Maybe.map DrawCard

        [ "skip", colorStr ] ->
            Color.fromString colorStr
                |> Maybe.map SkipCard

        [ "reverse", colorStr ] ->
            Color.fromString colorStr
                |> Maybe.map ReverseCard

        [ "wild", "draw", "four" ] ->
            Just (WildCard DrawFour)

        [ "wild" ] ->
            Just (WildCard Standard)

        numberStr :: colorStr :: [] ->
            let
                number =
                    String.toInt numberStr

                color =
                    Color.fromString colorStr
            in
            case ( number, color ) of
                ( Just num, Just col ) ->
                    Just (NumberCard num col)

                _ ->
                    Nothing

        _ ->
            Nothing


toInt : Card -> Int
toInt card =
    case card of
        NumberCard number color ->
            number + 100 * Color.toInt color

        DrawCard color ->
            10 + 100 * Color.toInt color

        SkipCard color ->
            11 + 100 * Color.toInt color

        ReverseCard color ->
            12 + 100 * Color.toInt color

        WildCard wildCard ->
            case wildCard of
                Standard ->
                    100 * 13

                DrawFour ->
                    100 * 14


getColor : Card -> Maybe Color
getColor card =
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
            getColor card

        ChoiceCard _ color ->
            Just color


canPlayCard : ( Maybe Card, Maybe Color ) -> Card -> Bool
canPlayCard ( maybeActiveCard, maybeActiveColor ) playedCard =
    case ( maybeActiveCard, maybeActiveColor ) of
        ( Just activeCard, Just activeColor ) ->
            case playedCard of
                WildCard _ ->
                    True

                NumberCard number color ->
                    case activeCard of
                        NumberCard activeNumber _ ->
                            number == activeNumber || color == activeColor

                        _ ->
                            color == activeColor

                DrawCard color ->
                    activeCard == DrawCard activeColor || color == activeColor

                SkipCard color ->
                    activeCard == SkipCard activeColor || color == activeColor

                ReverseCard color ->
                    activeCard == ReverseCard activeColor || color == activeColor

        _ ->
            False


getPlayableCards : ( Maybe Card, Maybe Color ) -> List Card -> List Card
getPlayableCards ( maybeActiveCard, maybeActiveColor ) cards =
    List.filter (canPlayCard ( maybeActiveCard, maybeActiveColor )) cards


sortCards : List Card -> List Card
sortCards cards =
    List.sortBy toInt cards


encodeCard : Card -> E.Value
encodeCard card =
    E.string (toString card)


decodeCard : D.Decoder Card
decodeCard =
    decodeMaybe D.string fromString


encodePlayableCard : PlayableCard -> E.Value
encodePlayableCard card =
    case card of
        StandardCard c ->
            E.object
                [ ( "card", E.string (toString c) )
                , ( "color", E.null )
                ]

        ChoiceCard c color ->
            E.object
                [ ( "card", E.string (toString c) )
                , ( "color", E.string (Color.toString color) )
                ]


decodePlayableCard : D.Decoder PlayableCard
decodePlayableCard =
    D.map2
        (\card color ->
            case color of
                Just c ->
                    ChoiceCard card c

                Nothing ->
                    StandardCard card
        )
        (D.field "card" decodeCard)
        (D.field "color" (D.nullable Color.decodeColor))
