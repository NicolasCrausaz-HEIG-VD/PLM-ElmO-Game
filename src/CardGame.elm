module CardGame exposing (..)

import List.Extra


type Color
    = Red
    | Blue
    | Green
    | Yellow


type Card
    = NumberCard Int Color
    | DrawCard Int Color
    | SkipCard Color
    | ReverseCard Color
    | WildCard


type alias Hand =
    List Card


type alias Draw =
    List Card


type alias Player =
    { name : String
    , hand : Hand
    }


type alias Game =
    { players : List Player
    , drawStack : Draw
    , activeCard : Maybe Card
    , activeColor : Maybe Color
    }



-- INIT


allCards : List Card
allCards =
    let
        colors =
            [ Red, Blue, Green, Yellow ]

        numbers =
            List.range 1 9
    in
    List.concatMap (\color -> List.map (\number -> NumberCard number color) numbers) colors
        ++ List.concatMap (\color -> List.map (\number -> DrawCard number color) numbers) colors
        ++ List.concatMap (\color -> List.map (\_ -> SkipCard color) numbers) colors
        ++ List.concatMap (\color -> List.map (\_ -> ReverseCard color) numbers) colors
        ++ List.map (\_ -> WildCard) (List.range 0 4)



-- LOGIC


addPlayer : String -> Game -> Game
addPlayer name game =
    let
        ( hand, drawStack ) =
            List.Extra.splitAt 7 game.drawStack
    in
    { game
        | players = game.players ++ [ { name = name, hand = hand } ]
        , drawStack = drawStack
    }



-- Return a game with the first card in the draw stack as the active card
-- Infinite loop if no card is playable fixing passing array in params


getFirstCard : Game -> Game
getFirstCard game =
    case game.drawStack of
        [] ->
            game

        card :: rest ->
            case card of
                NumberCard _ color ->
                    { game | activeCard = Just card, activeColor = Just color }

                _ ->
                    getFirstCard { game | drawStack = rest ++ [ card ] }



-- Return a game for the next turn


nextTurn : Game -> Game
nextTurn game =
    { game | players = nextPlayer game.players }


handleCardPlay : Card -> Game -> Game
handleCardPlay card game =
    case card of
        NumberCard _ color ->
            { game | activeCard = Just card, activeColor = Just color }

        DrawCard _ color ->
            { game | activeCard = Just card, activeColor = Just color }

        ReverseCard color ->
            { game | activeCard = Just card, activeColor = Just color }

        SkipCard color ->
            { game | activeCard = Just card, activeColor = Just color }

        WildCard ->
            { game | activeCard = Just card, activeColor = Nothing }


nextPlayer : List Player -> List Player
nextPlayer players =
    case players of
        [] ->
            []

        player :: rest ->
            rest ++ [ player ]

initGame : Game
initGame =
    { players = []
    , drawStack = allCards
    , activeCard = Nothing
    , activeColor = Nothing
    }
