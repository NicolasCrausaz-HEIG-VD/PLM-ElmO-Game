port module Pages.Room exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Dict
import Game.Card
import Game.CardView
import Game.Client
import Game.Color
import Game.Host
import Html exposing (Html, button, div, img, li, span, text)
import Html.Attributes exposing (class, classList, disabled, id, src, style)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Network
import Session exposing (Session)
import Utils exposing (Code, UUID)



-- MODEL


type GameState
    = Playing
    | SelectColor Game.Card.Card


type alias Model =
    { session : Session
    , state : GameState
    , game : Maybe Game.Client.Model
    }


initModel : Session -> Model
initModel session =
    { session = session, game = Nothing, state = Playing }


init : Code -> Session -> ( Model, Cmd Msg )
init code session =
    case session.session of
        Session.NotConnected ->
            ( initModel session, Network.joinRoom code )

        Session.Host _ data ->
            ( initModel session, outgoingAction (Game.Host.encodeAction (Game.Host.PlayerJoin data.playerUUID data.username)) )

        Session.Client data ->
            ( initModel session, outgoingAction (Game.Host.encodeAction (Game.Host.PlayerJoin data.playerUUID data.username)) )



-- PORTS


port incomingData : (E.Value -> msg) -> Sub msg


port outgoingAction : E.Value -> Cmd msg



--- UPDATE


type Msg
    = HostMsg Game.Host.HostMsg
    | ClientMsg ClientMsg
    | StartClientGame ( Code, UUID )


type ClientMsg
    = PlayCard Game.Card.PlayableCard
    | DrawCard
    | ClickCard Game.Card.Card
    | SetState GameState
    | IncomingData E.Value
    | SendAction Game.Host.Action


clientUpdate : ClientMsg -> Model -> ( Model, Cmd Msg )
clientUpdate msg model =
    case ( msg, model.game ) of
        ( DrawCard, Just game ) ->
            clientUpdate (SendAction (Game.Host.DrawCard game.localPlayer.uuid)) { model | state = Playing }

        ( PlayCard card, Just game ) ->
            clientUpdate (SendAction (Game.Host.PlayCard game.localPlayer.uuid card)) { model | state = Playing }

        ( ClickCard card, Just _ ) ->
            case card of
                Game.Card.WildCard _ ->
                    ( { model | state = SelectColor card }, Cmd.none )

                _ ->
                    clientUpdate (PlayCard (Game.Card.StandardCard card)) model

        ( SetState state, _ ) ->
            ( { model | state = state }, Cmd.none )

        ( SendAction action, _ ) ->
            ( model, outgoingAction (Game.Host.encodeAction action) )

        ( IncomingData data, _ ) ->
            case D.decodeValue Game.Client.decodeModel data of
                Ok newGame ->
                    ( { model | game = Just newGame }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HostMsg hostMsg ->
            Game.Host.update hostMsg model

        ClientMsg clientMsg ->
            clientUpdate clientMsg model

        StartClientGame ( code, playerUUID ) ->
            init code (model.session |> Session.update (Session.Client { code = code, playerUUID = playerUUID, username = "Elmo" }))



-- VIEW


displayDistantPlayer : Game.Client.Model -> Game.Client.DistantPlayer -> Html ClientMsg
displayDistantPlayer model player =
    div [ class "player", classList [ ( "active", model.currentPlayer == player.uuid ) ] ]
        [ span [ class "player-name" ] [ text player.name ]
        , div [ class "cards" ]
            (List.repeat player.cards (Game.CardView.emptyView [ class "card" ]))
        ]


displayPlayerDeck : Game.Client.Model -> Game.Client.LocalPlayer -> Html ClientMsg
displayPlayerDeck model player =
    div [ class "player-deck", classList [ ( "active", model.currentPlayer == player.uuid ) ] ]
        [ span [ class "player-name" ] [ text player.name ]
        , div [ class "cards" ]
            (List.map (\card -> Game.CardView.cardView [ class "card", onClick (ClickCard card), disabled (not (Game.Card.canPlayCard card ( model.activeCard, model.activeColor ))) ] card) (player.hand |> Game.Card.sortCards))
        ]


displayDrawStack : Int -> Html ClientMsg
displayDrawStack stack =
    div [ class "draw-stack" ]
        [ div [ class "cards" ]
            (List.repeat (min 3 stack) (Game.CardView.emptyView [ class "card", onClick DrawCard ]))
        ]


viewGame : Game.Client.Model -> Html ClientMsg
viewGame model =
    div [ class "game" ]
        [ div [ class "topbar" ] (List.map (displayDistantPlayer model) (Dict.values model.distantPlayers))
        , displayPlayerDeck model model.localPlayer
        , div [ class "center" ]
            [ displayDrawStack model.drawStack
            , div [ class "active-card" ]
                [ case model.activeCard of
                    Just card ->
                        Game.CardView.cardView [ class "card" ] card

                    Nothing ->
                        div [] []
                ]
            ]
        ]


viewChoice : Game.Client.Model -> Game.Card.Card -> Html ClientMsg
viewChoice game card =
    span []
        [ viewGame game
        , div [ class "modal" ]
            [ div [ class "modal-content" ]
                [ div [ class "title" ] [ text "Choose a color" ]
                , div [ class "cards-choice" ]
                    [ button [ class "card active", onClick (PlayCard (Game.Card.ChoiceCard card Game.Color.Red)) ]
                        [ img
                            [ src "/cards/empty_red.svg"
                            , style "height" "150px"
                            , class "card_front"
                            ]
                            []
                        ]
                    , button [ class "card active", onClick (PlayCard (Game.Card.ChoiceCard card Game.Color.Blue)) ]
                        [ img
                            [ src "/cards/empty_blue.svg"
                            , style "height" "150px"
                            , class "card_front"
                            ]
                            []
                        ]
                    , button [ class "card active", onClick (PlayCard (Game.Card.ChoiceCard card Game.Color.Green)) ]
                        [ img
                            [ src "/cards/empty_green.svg"
                            , style "height" "150px"
                            , class "card_front"
                            ]
                            []
                        ]
                    , button [ class "card active", onClick (PlayCard (Game.Card.ChoiceCard card Game.Color.Yellow)) ]
                        [ img
                            [ src "/cards/empty_yellow.svg"
                            , style "height" "150px"
                            , class "card_front"
                            ]
                            []
                        ]
                    ]
                , div [ class "modal-footer" ]
                    [ button [ id "close-modal", onClick (SetState Playing) ] [ text "Cancel" ]
                    ]
                ]
            ]
        ]


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Room Elmo"
    , content =
        case ( model.game, model.state ) of
            ( Just game, Playing ) ->
                viewGame game |> Html.map ClientMsg

            ( Just game, SelectColor card ) ->
                viewChoice game card |> Html.map ClientMsg

            ( Nothing, _ ) ->
                div [] [ text "Loading..." ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ incomingData (ClientMsg << IncomingData)
        , Game.Host.incomingAction (HostMsg << Game.Host.IncomingAction)
        , Network.joinedRoom StartClientGame
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
