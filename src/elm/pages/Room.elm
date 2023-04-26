port module Pages.Room exposing (..)

import Game.Card exposing (Card(..), PlayableCard(..))
import Game.CardView
import Game.Color
import Game.Game as Game
import Html exposing (Html, button, div, img, li, text, ul)
import Html.Attributes exposing (class, disabled, src, style)
import Html.Events exposing (onClick)
import Json.Decode exposing (Error(..))
import Random
import Route
import Session exposing (Session)



-- PORTS


port sendMsg : String -> Cmd msg


port handleMsg : (String -> msg) -> Sub msg



-- MODEL


type State
    = Idle
    | Choice Game.Player Card


type alias Model =
    { session : Session
    , game : Maybe Game.State
    , state : State
    }


defaultModel : Session -> Model
defaultModel session =
    { session = session, game = Nothing, state = Idle }


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
            (List.map (\card -> Game.CardView.view [] { size = "100px", flipped = False } card) player.hand)
        ]


displayPlayerDeck : Game.State -> Game.Player -> Html Msg
displayPlayerDeck game player =
    div [ class "player-deck" ]
        [ text player.name
        , div [ class "cards" ]
            (List.map (\card -> Game.CardView.view [ onClick (PlayCard player card), disabled (not (Game.canPlayCard card game)) ] { size = "150px", flipped = True } card) player.hand)
        ]


displayDrawStack : Game.Draw -> Html Msg
displayDrawStack stack =
    div [ class "draw-stack", onClick DrawCard ]
        [ div [ class "cards" ]
            (List.map (\card -> Game.CardView.view [] { size = "150px", flipped = False } card) stack)
        ]



-- UPDATE


type Msg
    = StartGame
    | BackLobby
    | PlayCard Game.Player Card
    | PlayPlayableCard Game.Player PlayableCard
    | DrawCard
    | NextTurn


playCard : Game.Player -> PlayableCard -> Game.State -> Game.State
playCard player playableCard game =
    Game.playerPlayCard player playableCard game
        |> Tuple.first
        |> Game.nextTurn
        |> Game.applyCardEffect (Game.Card.getCard playableCard)


playOrChoiceCard : Game.Player -> Card -> Game.State -> Model -> Model
playOrChoiceCard player card game model =
    case card of
        WildCard _ ->
            { model | state = Choice player card }

        _ ->
            { model | game = Just (playCard player (StandardCard card) game) }


updateGame : Msg -> Game.State -> Model -> ( Model, Cmd Msg )
updateGame msg game model =
    case msg of
        PlayCard player card ->
            ( playOrChoiceCard player card game model, Cmd.none )

        PlayPlayableCard player playableCard ->
            ( { model | game = Just (playCard player playableCard game), state = Idle }, Cmd.none )

        DrawCard ->
            ( { model | game = Just (Game.drawCard game |> Game.nextTurn) }, Cmd.none )

        NextTurn ->
            ( { model | game = Just (Game.nextTurn game) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


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
            updateGame msg game model



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
                            Game.CardView.view [] { size = "300px", flipped = True } card

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


viewChoice : Game.Player -> Card -> Game.State -> Html Msg
viewChoice player card _ =
    div [ class "choice-modal" ]
        [ div [ class "content" ]
            [ div [ class "title" ] [ text "Choose a color" ]
            , div []
                [ li
                    []
                    [ button [ class "card active", onClick (PlayPlayableCard player (ChoiceCard card Game.Color.Red)) ]
                        [ img
                            [ src "/cards/empty_red.svg"
                            , style "height" "150px"
                            , class "card_front"
                            ]
                            []
                        ]
                    ]
                , li
                    []
                    [ button [ class "card active", onClick (PlayPlayableCard player (ChoiceCard card Game.Color.Blue)) ]
                        [ img
                            [ src "/cards/empty_blue.svg"
                            , style "height" "150px"
                            , class "card_front"
                            ]
                            []
                        ]
                    ]
                , li
                    []
                    [ button [ class "card active", onClick (PlayPlayableCard player (ChoiceCard card Game.Color.Green)) ]
                        [ img
                            [ src "/cards/empty_green.svg"
                            , style "height" "150px"
                            , class "card_front"
                            ]
                            []
                        ]
                    ]
                , li
                    []
                    [ button [ class "card active", onClick (PlayPlayableCard player (ChoiceCard card Game.Color.Yellow)) ]
                        [ img
                            [ src "/cards/empty_yellow.svg"
                            , style "height" "150px"
                            , class "card_front"
                            ]
                            []
                        ]
                    ]
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
                case model.state of
                    Idle ->
                        viewGame game

                    Choice player card ->
                        viewChoice player card game
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
