module Game.Action exposing (..)

import Game.Card exposing (PlayableCard)
import Game.Core
import Json.Decode as D
import Json.Encode as E
import Utils exposing (UUID)


type alias Game =
    Game.Core.Model


type Action
    = PlayerJoin UUID String Bool
    | PlayerLeave UUID
    | PlayCard UUID PlayableCard
    | DrawCard UUID
    | SayUno UUID
    | Batch (List Action)


needToUpdate : Bool -> Game -> ( Game, Bool )
needToUpdate updated game =
    ( game, updated )


executeAction : Action -> Game -> ( Game, Bool )
executeAction action game =
    case action of
        PlayerJoin uuid name isAI ->
            game |> Game.Core.addPlayer ( uuid, name, isAI ) |> needToUpdate True

        PlayerLeave uuid ->
            game |> Game.Core.removePlayer uuid |> needToUpdate True

        PlayCard uuid card ->
            case game |> Game.Core.getPlayerIfTurn uuid |> Tuple.first of
                Just player ->
                    let
                        ( updatedGame, played ) =
                            Game.Core.playerPlayCard player card game
                    in
                    if played then
                        updatedGame
                            |> Game.Core.checkIfPreviousPlayerSaidUno
                            |> Game.Core.nextTurn
                            |> Game.Core.resetSaidUno
                            |> Game.Core.applyCardEffect (Game.Card.getCard card)
                            |> needToUpdate True

                    else
                        game |> needToUpdate False

                Nothing ->
                    game |> needToUpdate False

        DrawCard uuid ->
            case game |> Game.Core.getPlayerIfTurn uuid |> Tuple.first of
                Just player ->
                    game
                        |> Game.Core.checkIfPreviousPlayerSaidUno
                        |> Game.Core.drawCard 1 player
                        |> Game.Core.nextTurn
                        |> Game.Core.resetSaidUno
                        |> needToUpdate True

                Nothing ->
                    game |> needToUpdate False

        SayUno uuid ->
            case game |> Game.Core.getPlayer uuid |> Tuple.first of
                Just player ->
                    game
                        |> Game.Core.sayUndo player
                        |> needToUpdate True

                Nothing ->
                    game |> needToUpdate False

        Batch actions ->
            List.foldl
                (\innerAction ( innerGame, updated ) ->
                    let
                        ( newGame, newUpdated ) =
                            executeAction innerAction innerGame
                    in
                    ( newGame, updated || newUpdated )
                )
                ( game, False )
                actions


encodeAction : Action -> E.Value
encodeAction action =
    case action of
        PlayCard uuid card ->
            E.object
                [ ( "action", E.string "playCard" )
                , ( "uuid", E.string uuid )
                , ( "card", Game.Card.encodePlayableCard card )
                ]

        DrawCard uuid ->
            E.object
                [ ( "action", E.string "drawCard" )
                , ( "uuid", E.string uuid )
                ]

        PlayerJoin uuid name _ ->
            E.object
                [ ( "action", E.string "playerJoin" )
                , ( "uuid", E.string uuid )
                , ( "name", E.string name )
                ]

        PlayerLeave uuid ->
            E.object
                [ ( "action", E.string "playerLeave" )
                , ( "uuid", E.string uuid )
                ]

        SayUno uuid ->
            E.object
                [ ( "action", E.string "sayUno" )
                , ( "uuid", E.string uuid )
                ]

        Batch actions ->
            E.object
                [ ( "action", E.string "batch" )
                , ( "actions", E.list encodeAction actions )
                ]


decodeAction : D.Decoder Action
decodeAction =
    D.field "action" D.string
        |> D.andThen
            (\action ->
                case action of
                    "playCard" ->
                        D.map2 PlayCard
                            (D.field "uuid" D.string)
                            (D.field "card" Game.Card.decodePlayableCard)

                    "drawCard" ->
                        D.map DrawCard
                            (D.field "uuid" D.string)

                    "playerJoin" ->
                        D.map3 PlayerJoin
                            (D.field "uuid" D.string)
                            (D.field "name" D.string)
                            (False |> D.succeed)

                    "playerLeave" ->
                        D.map PlayerLeave
                            (D.field "uuid" D.string)

                    "sayUno" ->
                        D.map SayUno
                            (D.field "uuid" D.string)

                    "batch" ->
                        D.map Batch
                            (D.field "actions" (D.list decodeAction))

                    _ ->
                        D.fail ("Unknown action: " ++ action)
            )
