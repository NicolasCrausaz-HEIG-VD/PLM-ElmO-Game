port module Pages.Room exposing (..)

import Game.Card exposing (Card)
import Game.CardView as CardView
import Game.Game as Game
import Html exposing (Html, button, div, li, text, ul)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Random
import Route
import Session exposing (Session)



-- PORTS


port sendMsg : String -> Cmd msg


port handleMsg : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
    { session : Session
    , game : Maybe Game.State
    }


defaultModel : Session -> Model
defaultModel session =
    { session = session, game = Nothing }


init : Session -> ( Model, Cmd Msg )
init session =
    case session of
        Session.Connected _ _ ->
            ( defaultModel session, Cmd.none )

        _ ->
            -- If the session is not connected, redirect to lobby (middleware)
            ( defaultModel session, Route.replaceUrl (Session.navKey session) Route.Lobby )



-- VIEW


printPlayer : Game.Player -> Html Msg
printPlayer player =
    div [ class "player" ]
        [ text player.name
        , div [ class "cards" ]
            (List.map (\card -> CardView.view [] { size = "100px", flipped = False } card) player.hand)
        ]


displayPlayerDeck : Game.State -> Game.Player -> Html Msg
displayPlayerDeck game player =
    div [ class "player-deck" ]
        [ text player.name
        , div [ class "cards" ]
            (List.map (\card -> CardView.view [ onClick (PlayCard player card), disabled (not (Game.canPlayCard card game)) ] { size = "150px", flipped = True } card) player.hand)
        ]


displayDrawStack : Game.Draw -> Html Msg
displayDrawStack stack =
    div [ class "draw-stack", onClick DrawCard ]
        [ div [ class "cards" ]
            (List.map (\card -> CardView.view [] { size = "150px", flipped = True } card) stack)
        ]



-- VIEW HELPER FUNCTIONS


viewGame : Game.State -> Html Msg
viewGame game =
    case Game.getCurrentPlayer game of
        ( Just currentPlayer, otherPlayers ) ->
            div [ class "game" ]
                [ div [ class "topbar" ] (List.map printPlayer otherPlayers)
                , displayPlayerDeck game currentPlayer
                , div [ class "center" ]
                    [ displayDrawStack (List.take 3 game.drawStack)
                    , case game.activeCard of
                        Just card ->
                            CardView.view [] { size = "300px", flipped = True } card

                        Nothing ->
                            div [] []
                    ]
                ]

        ( Nothing, _ ) ->
            div [] []


viewLobby : Model -> Html Msg
viewLobby _ =
    div [ class "lobby-menu" ]
        [ div [ class "content" ]
            [ div [ class "title" ] [ text "Lobby" ]
            , div [ class "subtitle" ] [ text "Players: todo" ]
            , ul []
                [ li
                    [ class "active"
                    ]
                    [ button [ onClick StartGame ] [ text "Start Game" ] ]
                , li []
                    [ button [ onClick BackLobby ] [ text "Back to Lobby" ] ]
                ]
            ]
        ]



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Room"
    , content =
        case model.game of
            Nothing ->
                viewLobby model

            Just game ->
                viewGame game
    }



-- UPDATE


type Msg
    = StartGame
    | BackLobby
    | PlayCard Game.Player Card
    | DrawCard
    | NextTurn


updateGame : Msg -> Game.State -> ( Game.State, Cmd Msg )
updateGame msg game =
    case msg of
        PlayCard player card ->
            let
                playableCard =
                    Game.Card.makePlayableCard card
            in
            ( Game.playerPlayCard player playableCard game |> Tuple.first |> Game.nextTurn
            , Cmd.none
            )

        DrawCard ->
            ( Game.drawCard game |> Game.nextTurn, Cmd.none )

        NextTurn ->
            ( Game.nextTurn game, Cmd.none )

        _ ->
            ( game, Cmd.none )


updateNoGame : Msg -> Model -> ( Model, Cmd Msg )
updateNoGame msg model =
    case msg of
        StartGame ->
            ( { model
                | game =
                    Just
                        (Game.initGame
                            |> Game.shuffleGame (Random.initialSeed 5)
                            |> Game.addPlayer "Player 1"
                            |> Game.addPlayer "Player 2"
                            |> Game.addPlayer "Player 3"
                            |> Game.getFirstCard
                        )
              }
            , Cmd.none
            )

        BackLobby ->
            ( model, Route.replaceUrl (Session.navKey model.session) Route.Lobby )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.game of
        Nothing ->
            updateNoGame msg model

        Just game ->
            let
                ( newGame, cmd ) =
                    updateGame msg game
            in
            ( { model | game = Just newGame }, cmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
