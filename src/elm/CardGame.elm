module CardGame exposing (..)

import List.Extra
import Random
import Random.List


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

        numbers = 0 :: List.concatMap (\_ -> List.range 1 9) (List.range 1 2) -- one 0 and 2 of each number 1-9

        drawCards = 2 -- 2 for each color

        skipCards = 2 -- 2 for each color

        reverseCards = 2 -- 2 for each color

        wildCards = 4 -- 4 wild cards
    in
    List.concatMap (\color -> List.map (\number -> NumberCard number color) numbers) colors
        ++ List.concatMap (\color -> List.map (\_ -> DrawCard drawCards color) (List.range 1 drawCards)) colors
        ++ List.concatMap (\color -> List.map (\_ -> SkipCard color) (List.range 1 skipCards)) colors
        ++ List.concatMap (\color -> List.map (\_ -> ReverseCard color) (List.range 1 reverseCards)) colors
        ++ List.map (\_ -> WildCard) (List.range 1 wildCards)
    

shuffle : List e -> Random.Seed -> List e
shuffle list seed = (Random.step (Random.List.shuffle list) seed) |> Tuple.first




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
                    { game | activeCard = Just card, activeColor = Just color, drawStack = rest }

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

shuffleGame : Game -> Random.Seed -> Game
shuffleGame game seed =
    { game | drawStack = shuffle game.drawStack seed }


-- Return the current player or Nothing if no player is playing
getCurrentPlayer : Game -> Maybe Player
getCurrentPlayer game =
    case game.players of
        [] ->
            Nothing

        player :: _ ->
            Just player