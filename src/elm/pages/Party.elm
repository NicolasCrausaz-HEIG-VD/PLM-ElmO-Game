port module Pages.Party exposing (..)

import Dict exposing (Dict)
import Game.Card exposing (PlayableCard(..))
import Game.CardView
import Game.Client
import Game.Core
import Game.Host
import Game.JSON
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, disabled)
import Json.Decode as D
import Json.Encode as E
import Session exposing (Session)
import Html.Events exposing (onClick)



-- MODEL


type alias Model =
    { session : Session
    , host : Maybe Game.Host.Game
    , game : Maybe Game.Client.Model
    }


init : Session -> ( Model, Cmd Msg )
init session =
    case session of
        _ ->
            ( { session = session, game = Nothing, host = Nothing }, Game.Core.newGame StartHost )



-- PORTS


port incomingData : (E.Value -> msg) -> Sub msg


port outgoingData : E.Value -> Cmd msg


port incomingAction : (E.Value -> msg) -> Sub msg


port outgoingAction : E.Value -> Cmd msg



--- UPDATE

type Action
    = PlayCard PlayableCard
    | DrawCard

type Msg
    = IncomingData E.Value
    | SendAction Game.Host.Action
    | IncomingAction E.Value
    | SendData Game.Host.Game
    | StartHost Game.Host.Game
    | NoOp

toMsg : Model -> Action -> Msg
toMsg model action =
    case model.game of
        Just game ->
            case action of
                PlayCard card ->
                    SendAction (Game.Host.PlayCard game.localPlayer.uuid card)

                DrawCard ->
                    SendAction (Game.Host.DrawCard game.localPlayer.uuid)

        Nothing ->
            NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncomingData data ->
            case D.decodeValue Game.JSON.decodeModel data of
                Ok newGame ->
                    ( { model | game = Just newGame }, Cmd.none )

                Err err ->
                    ( model, Debug.log ("Error decoding incoming data: " ++ Debug.toString err) Cmd.none )

        SendAction action ->
            ( model, outgoingAction (Game.JSON.encodeAction action) )

        StartHost host ->
            let
                game =
                    host 
                        |> Game.Core.addPlayer ("uuid1", "Player 1")
                        |> Game.Core.addPlayer ("uuid2", "Player 2")
                        |> Game.Core.addPlayer ("uuid3", "Player 3")
                        |> Game.Core.addPlayer ("uuid4", "Player 4")
            in
            ( { model | host = Just game, game = Game.Host.toClient game "uuid1" }, Cmd.none )
            

        IncomingAction data ->
            case D.decodeValue Game.JSON.decodeAction data of
                Ok action ->
                    case model.host of
                        Just host ->
                            let
                                ( newHost, _ ) =
                                    Game.Host.onAction action host
                            in
                            ( { model | host = Just newHost, game = Game.Host.toClient newHost "uuid1" }, Cmd.none )

                        Nothing ->
                            ( model, Debug.log "Received action but no host is running" Cmd.none )

                Err err ->
                    ( model, Debug.log ("Error decoding incoming action: " ++ Debug.toString err) Cmd.none )

        SendData data ->
            case model.host of
                Just hostGame ->
                    case Game.Host.toClient hostGame "" of
                        Just clientGame ->
                            ( { model | game = Just clientGame }, outgoingData (Game.JSON.encodeModel clientGame) )

                        Nothing ->
                            ( model, Debug.log "Tried to send data but no host is running" Cmd.none )

                Nothing ->
                    ( model, Debug.log "Tried to send data but no host is running" Cmd.none )
        _ ->
            ( model, Cmd.none )



-- VIEW


displayDistantPlayer : Game.Client.DistantPlayer -> Html Msg
displayDistantPlayer player =
    div [ class "player" ]
        [ text player.name
        , div [ class "cards" ]
            (List.repeat player.cards (Game.CardView.emptyView [ class "card" ]))
        ]


displayPlayerDeck : Game.Client.LocalPlayer -> Game.Client.Model -> (Action -> Msg) ->  Html Msg
displayPlayerDeck player model sendAction =
    div [ class "player-deck" ]
        [ text player.name
        , div [ class "cards" ]
            (List.map (\card -> Game.CardView.cardView [ class "card", onClick (sendAction DrawCard), disabled (not (Game.Card.canPlayCard card ( model.activeCard, model.activeColor ))) ] card) player.hand)
        ]


displayDrawStack : Int -> (Action -> Msg) ->  Html Msg
displayDrawStack stack sendAction =
    div [ class "draw-stack" ]
        [ div [ class "cards" ]
            (List.repeat (min 3 stack) (Game.CardView.emptyView [ class "card", onClick (sendAction DrawCard) ]))
        ]


viewGame : Game.Client.Model -> (Action -> Msg) ->  Html Msg
viewGame model sendAction =
    div [ class "game" ]
        [ div [ class "topbar" ] (List.map displayDistantPlayer (Dict.values model.distantPlayers))
        , displayPlayerDeck model.localPlayer model sendAction
        , div [ class "center" ]
            [ displayDrawStack model.drawStack sendAction
            , div [ class "active-card" ]
                [ case model.activeCard of
                    Just card ->
                        Game.CardView.cardView [ class "card" ] card

                    Nothing ->
                        div [] []
                ]
            ]
        ]

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Party Elmo"
    , content =
        case model.game of
            Just game ->
                viewGame game (toMsg model)

            Nothing ->
                div [] [ text "Loading..." ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ incomingData IncomingData
        , incomingAction IncomingAction
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
