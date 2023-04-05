module Game.Game exposing (..)

import Game.Card exposing (Card(..), PlayableCard(..), WildCardType(..), getCardColor)
import Game.Color exposing (Color(..))
import List.Extra
import Random
import Random.List


type alias Hand =
    List Card


type alias Draw =
    List Card


type alias Player =
    { name : String
    , hand : Hand
    }


type alias State =
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
            0 :: List.concatMap (\_ -> List.range 1 9) (List.range 1 2)

        -- one 0 and 2 of each number 1-9
        drawCards =
            2

        -- 2 for each color
        skipCards =
            2

        -- 2 for each color
        reverseCards =
            2

        -- 2 for each color
        wildCards =
            4

        -- 4 wild cards
        wildDrawCards =
            4

        -- 4 wild draw cards
    in
    List.concatMap (\color -> List.map (\number -> NumberCard number color) numbers) colors
        ++ List.concatMap (\color -> List.map (\_ -> DrawCard color) (List.range 1 drawCards)) colors
        ++ List.concatMap (\color -> List.map (\_ -> SkipCard color) (List.range 1 skipCards)) colors
        ++ List.concatMap (\color -> List.map (\_ -> ReverseCard color) (List.range 1 reverseCards)) colors
        ++ List.map (\_ -> WildCard DrawFour) (List.range 1 wildDrawCards)
        ++ List.map (\_ -> WildCard Standard) (List.range 1 wildCards)


shuffle : List e -> Random.Seed -> List e
shuffle list seed =
    Random.step (Random.List.shuffle list) seed |> Tuple.first



-- LOGIC

addPlayer : String -> State -> State
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


getFirstCard : State -> State
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


nextTurn : State -> State
nextTurn game =
    { game | players = nextPlayer game.players }


canPlayCard : Card -> State -> Bool
canPlayCard playedCard game =
    case (playedCard, game.activeColor, game.activeCard) of
        (WildCard _, _, _) -> True

        (NumberCard number color, Just activeColor, Just activeCard) ->
            case activeCard of
                NumberCard activeNumber _ ->
                    number == activeNumber || color == activeColor

                _ ->
                    color == activeColor

        (DrawCard color, Just activeColor, Just activeCard) ->
            activeCard == DrawCard activeColor || color == activeColor

        (SkipCard color, Just activeColor, Just activeCard) ->
            activeCard == SkipCard activeColor || color == activeColor

        (ReverseCard color, Just activeColor, Just activeCard) ->
            activeCard == ReverseCard activeColor || color == activeColor

        _ ->
            False


playCard : PlayableCard -> State -> (State, Bool)
playCard playableCard game =
    let
        activeCard = Game.Card.getCard playableCard

        activeColor = case playableCard of
            StandardCard card -> getCardColor card
            ChoiceCard _ color -> Just color
    in
        if canPlayCard activeCard game then
            ({ game | activeCard = Just activeCard, activeColor = activeColor }, True)
        else
            (game, False)
playerPlayCard : Player -> PlayableCard -> State -> (State, Bool)
playerPlayCard player playableCard game =
    let
        (newState, success) =
            playCard playableCard game

        newPlayer =
            { player | hand = List.Extra.remove (Game.Card.getCard playableCard) player.hand }
    in
    if success then
        ({ newState | players = newPlayer :: newState.players }, True)
    else
        (game, False)

nextPlayer : List Player -> List Player
nextPlayer players =
    case players of
        [] ->
            []

        player :: rest ->
            rest ++ [ player ]


initGame : State
initGame =
    { players = []
    , drawStack = allCards
    , activeCard = Nothing
    , activeColor = Nothing
    }


shuffleGame : Random.Seed -> State -> State
shuffleGame seed game =
    { game | drawStack = shuffle game.drawStack seed }


getCurrentPlayer : State -> Maybe Player
getCurrentPlayer game =
    case game.players of
        [] ->
            Nothing

        player :: _ ->
            Just player

getPlayer : String -> State -> (Maybe Player, List Player)
getPlayer name game =
    let
        (matchingPlayer, remainingPlayers) =
            List.partition (\player -> player.name == name) game.players
    in
    (List.head matchingPlayer, remainingPlayers)



drawCard : State -> State
drawCard game =
    case game.drawStack of
        [] ->
            game

        card :: rest ->
            case getCurrentPlayer game of
                Nothing ->
                    game

                Just player ->
                    { game
                        | players = { player | hand = player.hand ++ [ card ] } :: game.players
                        , drawStack = rest
                    }
