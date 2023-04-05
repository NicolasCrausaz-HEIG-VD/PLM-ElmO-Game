port module Pages.Room exposing (..)

import Game.Card exposing (Card)
import Game.CardView as CardView
import Game.Game as Game
import Html exposing (Html, button, div, li, p, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random
import Route
import Session exposing (Session)
import Html.Attributes exposing (disabled)
import Game.Card exposing (PlayableCard)



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
        , div [class "cards"]
            (List.map (\card -> CardView.view [] { size = "100px", flipped = False } card) player.hand)
        ]


displayPlayerDeck : Game.State -> Game.Player -> Html Msg
displayPlayerDeck game player =
    div [ class "player-deck" ]
        [ text player.name
        , div [ class "cards" ]
                  (List.map (\card -> CardView.view [ onClick (PlayCard player card), disabled (not (Game.canPlayCard card game)  ) ] { size = "150px", flipped = True } card) player.hand)
        ]


displayDrawStack : Game.Draw -> Html Msg
displayDrawStack stack =
    div [ class "draw-stack", onClick DrawCard ]
        [ div [ class "cards" ]
            (List.map (\card -> CardView.view [] { size = "150px", flipped = True } card) stack)
        ]


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Room"
    , content =
        case model.game of
            Nothing ->
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

            Just game ->
                case Game.getPlayer "Player 1" game of
                    (Just currentPlayer, otherPlayers) ->
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
                    (Nothing, _) ->
                        div [] []
    }



-- UPDATE


type Msg
    = StartGame
    | BackLobby
    | PlayCard Game.Player Card
    | DrawCard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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

        PlayCard player card -> 
            let
                playableCard = Game.Card.makePlayableCard card
            in
                ( { model
                    | game = Maybe.map (\game -> (Game.playerPlayCard player playableCard game) |> Tuple.first) model.game
                }
                , Cmd.none
                )
        

        DrawCard -> ( {
            model
                | game = Maybe.map Game.drawCard model.game
            }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
