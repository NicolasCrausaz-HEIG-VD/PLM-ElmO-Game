module Game.Game exposing (..)

import Game.Card exposing (Card(..), PlayableCard(..), WildCardType(..))
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

        createCards n f =
            List.concatMap (\color -> List.map (\_ -> f color) (List.range 1 n)) colors

        drawCards =
            createCards 2 DrawCard

        skipCards =
            createCards 2 SkipCard

        reverseCards =
            createCards 2 ReverseCard

        wildDrawCards =
            List.repeat 4 (WildCard DrawFour)

        wildCards =
            List.repeat 4 (WildCard Standard)
    in
    List.concatMap (\color -> List.map (\number -> NumberCard number color) numbers) colors
        ++ drawCards
        ++ skipCards
        ++ reverseCards
        ++ wildDrawCards
        ++ wildCards


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



nextTurn : State -> State
nextTurn game =
    { game | players = nextPlayer game.players }


canPlayCard : Card -> State -> Bool
canPlayCard playedCard game =
    case ( playedCard, game.activeColor, game.activeCard ) of
        ( WildCard _, _, _ ) ->
            True

        ( NumberCard number color, Just activeColor, Just activeCard ) ->
            case activeCard of
                NumberCard activeNumber _ ->
                    number == activeNumber || color == activeColor

                _ ->
                    color == activeColor

        ( DrawCard color, Just activeColor, Just activeCard ) ->
            activeCard == DrawCard activeColor || color == activeColor

        ( SkipCard color, Just activeColor, Just activeCard ) ->
            activeCard == SkipCard activeColor || color == activeColor

        ( ReverseCard color, Just activeColor, Just activeCard ) ->
            activeCard == ReverseCard activeColor || color == activeColor

        _ ->
            False


playCard : PlayableCard -> State -> ( State, Bool )
playCard playableCard game =
    let
        activeCard =
            Game.Card.getCard playableCard
    in
    if canPlayCard activeCard game then
        ( { game
            | activeCard = Just activeCard
            , activeColor = Game.Card.getPlayableCardColor playableCard
            , drawStack = addCardToDrawStack game.activeCard game.drawStack
          }
        , True
        )

    else
        ( game, False )


updatePlayer : Player -> List Player -> List Player
updatePlayer updatedPlayer players =
    List.map
        (\player ->
            if player.name == updatedPlayer.name then
                updatedPlayer

            else
                player
        )
        players


playerPlayCard : Player -> PlayableCard -> State -> ( State, Bool )
playerPlayCard player playableCard game =
    let
        ( newState, success ) =
            playCard playableCard game

        newPlayer =
            { player | hand = List.Extra.remove (Game.Card.getCard playableCard) player.hand }
    in
    if success then
        ( { newState | players = updatePlayer newPlayer newState.players }, True )

    else
        ( game, False )


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


getCurrentPlayer : State -> ( Maybe Player, List Player )
getCurrentPlayer game =
    case game.players of
        [] ->
            ( Nothing, [] )

        currentPlayer :: remainingPlayers ->
            ( Just currentPlayer, remainingPlayers )


getPlayer : String -> State -> ( Maybe Player, List Player )
getPlayer name game =
    let
        ( matchingPlayer, remainingPlayers ) =
            List.partition (\player -> player.name == name) game.players
    in
    ( List.head matchingPlayer, remainingPlayers )


drawCard : State -> State
drawCard game =
    case game.drawStack of
        [] ->
            game

        card :: rest ->
            let
                ( currentPlayer, remainingPlayers ) =
                    getCurrentPlayer game
            in
            case currentPlayer of
                Nothing ->
                    game

                Just player ->
                    { game
                        | players = { player | hand = player.hand ++ [ card ] } :: remainingPlayers
                        , drawStack = rest
                    }


addCardToDrawStack : Maybe Card -> Draw -> Draw
addCardToDrawStack cardToAdd drawStack =
    case cardToAdd of
        Nothing ->
            drawStack

        Just card ->
            drawStack ++ [ card ]
