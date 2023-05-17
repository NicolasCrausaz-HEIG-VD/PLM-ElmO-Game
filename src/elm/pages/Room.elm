port module Pages.Room exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Game.Action
import Game.Card
import Game.CardView
import Game.Client
import Game.Color
import Game.Host
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (attribute, class, classList, disabled, id, src, style)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Network
import Random
import Route
import Session exposing (Session)
import Utils exposing (Code, UUID)



-- MODEL


type GameState
    = Playing
    | SelectColor Game.Card.Card
    | GameOver


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

        Session.Host config data ->
            ( initModel session
            , Cmd.batch
                [ Cmd.map HostMsg (Game.Host.actionCmd (Game.Action.PlayerJoin data.playerUUID data.username False))
                , Cmd.map HostMsg (Game.Host.addAIPlayersCmd config.nbAI)
                ]
            )

        Session.Client data ->
            ( initModel session, outgoingAction (Game.Action.encodeAction (Game.Action.PlayerJoin data.playerUUID data.username False)) )



-- PORTS


port incomingData : (E.Value -> msg) -> Sub msg


port outgoingAction : E.Value -> Cmd msg



--- UPDATE


type Msg
    = HostMsg Game.Host.HostMsg
    | ClientMsg ClientMsg
    | ConnectClientGame ( Code, UUID, Bool )
    | StartClientGame ( Code, UUID, String )
    | LostConnection Code


type ClientMsg
    = PlayCard Game.Card.PlayableCard
    | DrawCard
    | ClickCard Game.Card.Card
    | SetState GameState
    | IncomingData E.Value
    | SendAction Game.Action.Action
    | GoTo Route.Route
    | SayUno


clientUpdate : ClientMsg -> Model -> ( Model, Cmd Msg )
clientUpdate msg model =
    case ( msg, model.game ) of
        ( DrawCard, Just game ) ->
            clientUpdate (SendAction (Game.Action.DrawCard game.localPlayer.uuid)) { model | state = Playing }

        ( PlayCard card, Just game ) ->
            clientUpdate (SendAction (Game.Action.PlayCard game.localPlayer.uuid card)) { model | state = Playing }

        ( SayUno, Just game ) ->
            clientUpdate (SendAction (Game.Action.SayUno game.localPlayer.uuid)) { model | state = Playing }

        ( ClickCard card, Just _ ) ->
            case card of
                Game.Card.WildCard _ ->
                    ( { model | state = SelectColor card }, Cmd.none )

                _ ->
                    clientUpdate (PlayCard (Game.Card.StandardCard card)) model

        ( SetState state, _ ) ->
            ( { model | state = state }, Cmd.none )

        ( GoTo route, _ ) ->
            ( model, Route.replaceUrl model.session.key route )

        ( SendAction action, _ ) ->
            ( model, outgoingAction (Game.Action.encodeAction action) )

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
            let
                ( updatedModel, cmd ) =
                    Game.Host.update hostMsg model
            in
            ( updatedModel, Cmd.map HostMsg cmd )

        ClientMsg clientMsg ->
            clientUpdate clientMsg model

        ConnectClientGame ( code, playerUUID, success ) ->
            if success then
                ( model, Random.generate (\username -> StartClientGame ( code, playerUUID, username )) Utils.randomCharacterGenerator )

            else
                ( model, Route.replaceUrl model.session.key Route.Lobby )

        StartClientGame ( code, playerUUID, username ) ->
            init code (model.session |> Session.update (Session.Client { code = code, playerUUID = playerUUID, username = username }))

        LostConnection _ ->
            ( model, Route.replaceUrl model.session.key Route.Lobby )


getGameState : Model -> GameState
getGameState model =
    case model.game of
        Just game ->
            if game.gameOver then
                GameOver

            else
                model.state

        Nothing ->
            model.state



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
            (List.map (\card -> Game.CardView.cardView [ class "card", onClick (ClickCard card), disabled (not (Game.Client.hintPlayCard model card)) ] card) (player.hand |> Game.Card.sortCards))
        ]


displayDrawStack : Int -> Bool -> Html ClientMsg
displayDrawStack stack hint =
    div [ class "draw-stack", classList [ ( "hint", hint ) ] ]
        [ div [ class "cards" ]
            (List.repeat (min 3 stack) (Game.CardView.emptyView [ class "card", onClick DrawCard ]))
        ]


viewGame : Game.Client.Model -> Html ClientMsg
viewGame model =
    div [ class "game" ]
        [ div [ class "topbar" ] (List.map (displayDistantPlayer model) model.distantPlayers)
        , displayPlayerDeck model model.localPlayer
        , div [ class "center" ]
            [ displayDrawStack model.drawStack (model |> Game.Client.hintDrawCard)
            , div [ class "active-card" ]
                [ case ( model.activeCard, model.activeColor ) of
                    ( Just card, Just color ) ->
                        Game.CardView.cardView [ class "card", attribute "data-color" (color |> Game.Color.toString) ] card

                    _ ->
                        div [] []
                ]
            ]
        , if Game.Client.showUnoButton model then
            button [ id "uno-button", onClick SayUno ] []

          else
            div [] []
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


viewGameOver : Game.Client.Model -> Html ClientMsg
viewGameOver model =
    div [ class "modal" ]
        [ div [ class "modal-content" ]
            [ img [ src "/GameOver.svg", class "logo" ] []
            , button [ onClick (GoTo Route.Lobby) ] [ text "Back to lobby" ]
            ]
        ]


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Room Elmo"
    , content =
        case ( model.game, getGameState model ) of
            ( Just game, Playing ) ->
                viewGame game |> Html.map ClientMsg

            ( Just game, SelectColor card ) ->
                viewChoice game card |> Html.map ClientMsg

            ( Just game, GameOver ) ->
                viewGameOver game |> Html.map ClientMsg

            _ ->
                div [] [ text "Loading..." ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ incomingData (ClientMsg << IncomingData)
        , Game.Host.incomingAction (HostMsg << Game.Host.IncomingAction)
        , Network.joinedRoom ConnectClientGame
        , Network.lostConnection LostConnection
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
