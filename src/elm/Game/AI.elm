module Game.AI exposing (aiBatch, getBestAction)

import Game.Action exposing (Action)
import Game.Card as Card exposing (Card, PlayableCard(..))
import Game.Color as Color
import Game.Core exposing (Player)
import Platform exposing (Task)
import Process
import Random
import Task
import Time


type alias Game =
    Game.Core.Model


aiBatch : (Result String Action -> msg) -> Game -> Cmd msg
aiBatch msg newHost =
    newHost
        |> aiTurn
        |> List.map (Task.attempt msg)
        |> Cmd.batch


aiTurn : Game -> List (Task String Action)
aiTurn game =
    case Game.Core.getCurrentPlayer game of
        ( Just currentPlayer, others ) ->
            if currentPlayer.isAI then
                onCurrentTurn currentPlayer game :: mapAI (onEachTurn game) others

            else
                mapAI (onEachTurn game) others

        _ ->
            []


mapAI : (Player -> Maybe (Task String Action)) -> List Player -> List (Task String Action)
mapAI action players =
    players
        |> List.filter .isAI
        |> List.filterMap action


randomToTask : Random.Generator a -> Task Never a
randomToTask generator =
    Time.now
        |> Task.map (Tuple.first << Random.step generator << Random.initialSeed << Time.posixToMillis)


createDelayTask : Int -> Int -> Task String ()
createDelayTask minDelay maxDelay =
    let
        delayGenerator =
            Random.int minDelay maxDelay
    in
    randomToTask delayGenerator
        |> Task.andThen
            (\delay ->
                Process.sleep (toFloat delay)
            )
        |> Task.mapError (\_ -> "Impossible error")


onEachTurn : Game -> Player -> Maybe (Task String Action)
onEachTurn _ player =
    if not player.saidUno && List.length player.hand == 1 then
        Just
            (createDelayTask 250 2000
                |> Task.andThen (\_ -> Task.succeed (Game.Action.SayUno player.uuid))
            )

    else
        Nothing


onCurrentTurn : Player -> Game -> Task String Action
onCurrentTurn player game =
    createDelayTask 400 3000
        |> Task.andThen
            (\_ -> Task.succeed (getBestAction player game))


filterColorCards : List Card -> List ( Card, Color.Color )
filterColorCards cards =
    cards |> List.filterMap (\card -> Maybe.map (\color -> ( card, color )) (Card.getColor card))


getBestColor : List ( Card, Color.Color ) -> Color.Color
getBestColor cards =
    cards
        |> List.map Tuple.second
        |> Color.getMostCommonColor


getPlayableCard : Color.Color -> Card -> PlayableCard
getPlayableCard color card =
    case card of
        Card.WildCard _ ->
            ChoiceCard card color

        _ ->
            StandardCard card


getBestCard : Player -> Game -> Maybe PlayableCard
getBestCard player game =
    let
        playableCards =
            player.hand |> Game.Core.getPlayableCards game

        filterCards =
            filterColorCards playableCards

        bestColor =
            getBestColor filterCards

        bestColorCards =
            filterCards
                |> List.filterMap
                    (\( card, cardColor ) ->
                        if cardColor == bestColor then
                            Just card

                        else
                            Nothing
                    )
    in
    case playableCards of
        [] ->
            Nothing

        _ ->
            if List.length bestColorCards > 3 then
                bestColorCards
                    |> List.sortBy cardPriority
                    |> List.head
                    |> Maybe.map (getPlayableCard bestColor)

            else
                playableCards
                    |> List.sortBy cardPriority
                    |> List.head
                    |> Maybe.map (getPlayableCard (getBestColor (filterColorCards player.hand)))


cardPriority : Card -> Int
cardPriority card =
    case card of
        Card.WildCard wildType ->
            case wildType of
                Card.DrawFour ->
                    0

                Card.Standard ->
                    1

        Card.DrawCard _ ->
            2

        Card.SkipCard _ ->
            3

        Card.ReverseCard _ ->
            3

        _ ->
            4


getBestAction : Player -> Game -> Action
getBestAction player game =
    case getBestCard player game of
        Just card ->
            Game.Action.PlayCard player.uuid card

        Nothing ->
            Game.Action.DrawCard player.uuid
