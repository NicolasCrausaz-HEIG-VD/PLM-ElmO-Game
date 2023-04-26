module Game.Host exposing (..)

import Dict
import Game.Card exposing (PlayableCard)
import Game.Client
import Game.Core exposing (UUID)
import Pages.Room exposing (Msg(..))


type alias Game =
    Game.Core.Model


type Action
    = PlayerJoin ( UUID, String )
    | PlayerLeave UUID
    | PlayCard UUID PlayableCard
    | DrawCard UUID


update : Bool -> Game -> ( Game, Bool )
update updated game =
    ( game, updated )


onAction : Action -> Game -> ( Game, Bool )
onAction action game =
    case action of
        PlayerJoin player ->
            game |> Game.Core.addPlayer player |> update True

        PlayerLeave uuid ->
            game |> Game.Core.removePlayer uuid |> update True

        PlayCard uuid card ->
            case game |> Game.Core.getPlayerIfTurn uuid |> Tuple.first of
                Just player ->
                    let
                        ( updatedGame, played ) =
                            Game.Core.playerPlayCard player card game
                    in
                    if played then
                        updatedGame
                            |> Game.Core.nextTurn
                            |> Game.Core.applyCardEffect (Game.Card.getCard card)
                            |> update True

                    else
                        game |> update False

                Nothing ->
                    game |> update False

        DrawCard uuid ->
            case game |> Game.Core.getPlayerIfTurn uuid |> Tuple.first of
                Just player ->
                    game
                        |> Game.Core.drawCard 1 player
                        |> Game.Core.nextTurn
                        |> update True

                Nothing ->
                    game |> update False



--- Return the gGame.Client.Model for a given player ( local player )


toLocalPlayer : Game.Core.Player -> Game.Client.LocalPlayer
toLocalPlayer player =
    { name = player.name
    , uuid = player.uuid
    , hand = player.hand
    }


toDistantePlayer : Game.Core.Player -> Game.Client.DistantPlayer
toDistantePlayer player =
    { name = player.name
    , uuid = player.uuid
    , cards = List.length player.hand
    }


toClient : Game -> UUID -> Maybe Game.Client.Model
toClient game playerUUID =
    case ( game |> Game.Core.getPlayer playerUUID, game |> Game.Core.getCurrentPlayer |> Tuple.first ) of
        ( ( Just player, players ), Just currentPlayer ) ->
            Just
                { distantPlayers = Dict.fromList (List.map (\p -> ( p.uuid, toDistantePlayer p )) players)
                , localPlayer = toLocalPlayer player
                , currentPlayer = currentPlayer.uuid
                , drawStack = List.length game.drawStack
                , activeCard = game.activeCard
                , activeColor = game.activeColor
                }

        _ ->
            Nothing
