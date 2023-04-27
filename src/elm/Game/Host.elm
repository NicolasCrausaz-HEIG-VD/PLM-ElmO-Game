port module Game.Host exposing (..)

import Dict
import Game.Card exposing (PlayableCard)
import Game.Client
import Game.Core
import Pages.Room exposing (Msg(..))
import Utils exposing (UUID)
import Session exposing (SessionType)
import Json.Decode as D
import Json.Encode as E
import Session exposing (Session)


type alias Game =
    Game.Core.Model


type Action
    = PlayerJoin UUID String
    | PlayerLeave UUID
    | PlayCard UUID PlayableCard
    | DrawCard UUID


needToUpdate : Bool -> Game -> ( Game, Bool )
needToUpdate updated game =
    ( game, updated )


onAction : Action -> Game -> ( Game, Bool )
onAction action game =
    case action of
        PlayerJoin uuid name ->
            game |> Game.Core.addPlayer (uuid, name) |> needToUpdate True

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
                            |> Game.Core.nextTurn
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
                        |> Game.Core.drawCard 1 player
                        |> Game.Core.nextTurn
                        |> needToUpdate True

                Nothing ->
                    game |> needToUpdate False



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


port outgoingData : E.Value -> Cmd msg


port incomingAction : (E.Value -> msg) -> Sub msg


type HostMsg
    = IncomingAction E.Value
    | SendData Game



setHostGame : Game.Core.Model -> { a | session : Session } -> { a | session : Session }
setHostGame game model =
    case model.session.session of
        Session.Host _ roomData ->
            { model | session = model.session |> Session.update (Session.Host game roomData) }
        _ ->
            model

getHostGame : { a | session : Session } -> Maybe Game.Core.Model
getHostGame model =
    case model.session.session of
        Session.Host game _ ->
            Just game
        _ ->
            Nothing


update : HostMsg -> { a | session : Session } -> ({ a | session : Session }, Cmd msg)
update msg model =
    case ( msg, model |> getHostGame ) of
        ( IncomingAction data, Just host ) ->
            case D.decodeValue decodeAction data of
                Ok action ->
                    case onAction action host of
                        ( newHost, True ) ->
                            update (SendData newHost) model

                        ( newHost, _ ) ->
                            ( model |> setHostGame newHost, Cmd.none )

                Err err ->
                    ( model, Debug.log ("Error decoding incoming action: " ++ Debug.toString err) Cmd.none )

        ( SendData host, Just _ ) ->
            case toClient host "uuid1" of
                Just clientGame ->
                    ( model |> setHostGame host, outgoingData (Game.Client.encodeModel clientGame) )

                Nothing ->
                    ( model, Debug.log "Tried to send data but no client game was available" Cmd.none )

        _ ->
            ( model, Cmd.none )



-- DECODERS

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

        PlayerJoin uuid name ->
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
                        D.map2 PlayerJoin
                            (D.field "uuid" D.string)
                            (D.field "name" D.string)
                    
                    "playerLeave" ->
                        D.map PlayerLeave
                            (D.field "uuid" D.string)
                    _ ->
                        D.fail ("Unknown action: " ++ action)
            )
