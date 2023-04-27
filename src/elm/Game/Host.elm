port module Game.Host exposing (..)

import Game.Card exposing (PlayableCard)
import Game.Color
import Game.Core
import Json.Decode as D
import Json.Encode as E
import Session exposing (Session)
import Utils exposing (UUID)


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
            game |> Game.Core.addPlayer ( uuid, name ) |> needToUpdate True

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


getCurrentPlayer : { a | session : Session } -> UUID
getCurrentPlayer model =
    case model.session.session of
        Session.Host _ roomData ->
            roomData.playerUUID

        Session.Client roomData ->
            roomData.playerUUID

        _ ->
            ""


update : HostMsg -> { a | session : Session } -> ( { a | session : Session }, Cmd msg )
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

                Err _ ->
                    ( model, Cmd.none )

        ( SendData host, Just _ ) ->
            ( model |> setHostGame host, outgoingData (encodeGame host) )

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


encodePlayer : Game.Core.Player -> E.Value
encodePlayer player =
    E.object
        [ ( "name", E.string player.name )
        , ( "uuid", E.string player.uuid )
        , ( "hand", E.list Game.Card.encodeCard player.hand )
        ]


encodeGame : Game -> E.Value
encodeGame game =
    E.object
        [ ( "players", E.list encodePlayer game.players )
        , ( "currentPlayer", Utils.maybeEncode E.string (Maybe.map (\p -> p.uuid) (game |> Game.Core.getCurrentPlayer |> Tuple.first)) )
        , ( "drawStack", E.int (game.drawStack |> List.length) )
        , ( "activeCard", Utils.maybeEncode Game.Card.encodeCard game.activeCard )
        , ( "activeColor", Utils.maybeEncode Game.Color.encodeColor game.activeColor )
        ]
