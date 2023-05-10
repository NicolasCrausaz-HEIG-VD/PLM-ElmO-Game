module Game.Core exposing (..)

import Game.Card exposing (Card(..), PlayableCard(..), WildCardType(..))
import Game.Color exposing (Color(..))
import List.Extra
import Random
import Random.List
import Utils exposing (UUID)


type alias Hand =
    List Card


type alias Draw =
    List Card


type alias Player =
    { name : String
    , uuid : UUID
    , hand : Hand
    , saidUno : Bool
    }


type alias Model =
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


addPlayer : ( UUID, String ) -> Model -> Model
addPlayer ( uuid, name ) game =
    let
        ( hand, drawStack ) =
            List.Extra.splitAt 7 game.drawStack
    in
    { game
        | players = game.players ++ [ { name = name, hand = hand, uuid = uuid, saidUno = False } ]
        , drawStack = drawStack
    }


removePlayer : UUID -> Model -> Model
removePlayer uuid game =
    case getPlayer uuid game of
        ( Just player, remainingPlayers ) ->
            { game | players = remainingPlayers, drawStack = game.drawStack ++ player.hand }

        _ ->
            game


getFirstCard : Model -> Model
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


canPlayCard : Card -> Model -> Bool
canPlayCard card game =
    Game.Card.canPlayCard card ( game.activeCard, game.activeColor )


playCard : PlayableCard -> Model -> ( Model, Bool )
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


applyCardEffect : Card -> Model -> Model
applyCardEffect card game =
    case ( card, game |> getCurrentPlayer |> Tuple.first ) of
        ( DrawCard _, Just player ) ->
            drawCard 2 player game

        ( WildCard DrawFour, Just player ) ->
            drawCard 4 player game

        ( SkipCard _, _ ) ->
            nextTurn game

        ( ReverseCard _, _ ) ->
            { game | players = List.reverse game.players } |> nextTurn

        _ ->
            game


updatePlayer : Player -> List Player -> List Player
updatePlayer updatedPlayer players =
    List.map
        (\player ->
            if player.uuid == updatedPlayer.uuid then
                updatedPlayer

            else
                player
        )
        players


playerPlayCard : Player -> PlayableCard -> Model -> ( Model, Bool )
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


nextTurn : Model -> Model
nextTurn game =
    { game | players = nextPlayer game.players }


nextPlayer : List Player -> List Player
nextPlayer players =
    case players of
        [] ->
            []

        player :: rest ->
            ifPlayerHasNoCardSkip (rest ++ [ player ])


checkIfPreviousPlayerSaidUno : Model -> Model
checkIfPreviousPlayerSaidUno game =
    case game.players of
        [] ->
            game

        _ :: remainingPlayers ->
            case List.Extra.last remainingPlayers of
                Just player ->
                    if List.length player.hand == 1 && not player.saidUno then
                        drawCard 2 player game

                    else
                        game

                _ ->
                    game


resetSaidUno : Model -> Model
resetSaidUno game =
    { game | players = List.map (\player -> { player | saidUno = False }) game.players }


sayUndo : Player -> Model -> Model
sayUndo player game =
    { game | players = updatePlayer { player | saidUno = True } game.players }


ifPlayerHasNoCardSkip : List Player -> List Player
ifPlayerHasNoCardSkip players =
    case players of
        [] ->
            []

        player :: _ ->
            if List.isEmpty player.hand then
                nextPlayer players

            else
                players


emptyGame : Model
emptyGame =
    { players = []
    , drawStack = allCards
    , activeCard = Nothing
    , activeColor = Nothing
    }


newGame : (Model -> msg) -> Cmd msg
newGame msg =
    let
        updateModel shuffledCards =
            { emptyGame | drawStack = shuffledCards } |> getFirstCard
    in
    Random.generate (msg << updateModel) (Random.List.shuffle allCards)


getCurrentPlayer : Model -> ( Maybe Player, List Player )
getCurrentPlayer game =
    case game.players of
        [] ->
            ( Nothing, [] )

        currentPlayer :: remainingPlayers ->
            ( Just currentPlayer, remainingPlayers )


getPlayer : UUID -> Model -> ( Maybe Player, List Player )
getPlayer uuid game =
    let
        ( matchingPlayer, remainingPlayers ) =
            List.partition (\player -> player.uuid == uuid) game.players
    in
    ( List.head matchingPlayer, remainingPlayers )


getPlayerIfTurn : UUID -> Model -> ( Maybe Player, List Player )
getPlayerIfTurn uuid game =
    case getCurrentPlayer game of
        ( Just currentPlayer, remainingPlayers ) ->
            if currentPlayer.uuid == uuid then
                ( Just currentPlayer, remainingPlayers )

            else
                ( Nothing, game.players )

        _ ->
            ( Nothing, game.players )


drawCard : Int -> Player -> Model -> Model
drawCard n player game =
    let
        ( cards, drawStack ) =
            List.Extra.splitAt n game.drawStack
    in
    { game
        | players = updatePlayer { player | hand = player.hand ++ cards } game.players
        , drawStack = drawStack
    }


addCardToDrawStack : Maybe Card -> Draw -> Draw
addCardToDrawStack cardToAdd drawStack =
    case cardToAdd of
        Nothing ->
            drawStack

        Just card ->
            drawStack ++ [ card ]


isGameOver : Model -> Bool
isGameOver game =
    game.players
        |> List.filter (\player -> not (List.isEmpty player.hand))
        |> List.length
        |> (\length -> length <= 1)
