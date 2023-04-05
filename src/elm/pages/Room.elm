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


printCards : List Card -> Bool -> Html Msg
printCards cards flipped =
    div [ class "cards" ]
        (List.map (\card -> CardView.view [ onClick (CardClicked card) ] { size = "100px", flipped = flipped } card) cards)


printPlayer : Game.Player -> Html Msg
printPlayer player =
    div [ class "player" ]
        [ text player.name
        , div []
            [ printCards player.hand False
            ]
        ]


displayPlayerDeck : Game.Player -> Html Msg
displayPlayerDeck player =
    div [ class "player-deck" ]
        [ text player.name
        , div []
            [ printCards player.hand True ]
        ]


displayDrawStack : Game.Draw -> Html Msg
displayDrawStack stack =
    div [ class "draw-stack" ]
        [ printCards stack False ]


otherPlayers : List Game.Player -> Game.Player -> List Game.Player
otherPlayers players player =
    List.filter (\p -> p.name /= player.name) players


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
                div [ class "game" ]
                    [ div [ class "topbar" ] (List.map printPlayer (otherPlayers game.players (Game.getPlayer "Player 1" game |> Maybe.withDefault { name = "", hand = [] })))
                    , case Game.getPlayer "Player 1" game of
                        Just player ->
                            displayPlayerDeck player

                        Nothing ->
                            div [] []
                    , div [ class "center" ]
                        [ displayDrawStack (List.take 3 game.drawStack)
                        , case game.activeCard of
                            Just card ->
                                printCards [ card ] True

                            Nothing ->
                                div [] []
                        ]
                    ]
    }



-- UPDATE


type Msg
    = StartGame
    | BackLobby
    | CardClicked Card


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

        CardClicked card ->
            let
                _ =
                    Debug.log "CardClicked" card
            in
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
