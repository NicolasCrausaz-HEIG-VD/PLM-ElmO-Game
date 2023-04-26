port module Pages.Room exposing (..)

import Game.Card exposing (Card(..), PlayableCard(..))
import Game.CardView
import Game.Color
import Game.Core as Game
import Html exposing (Html, button, div, li, text, ul)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Json.Decode exposing (Error(..))
import Route
import Session exposing (Session)



-- PORTS


port sendMsg : String -> Cmd msg


port handleMsg : (String -> msg) -> Sub msg



-- MODEL


type GameState
    = Idle
    | Choice Game.Player Card


type alias GameModel =
    { model : Game.Model
    , state : GameState
    }


type State
    = Lobby
    | Game GameModel


type alias Model =
    { session : Session
    , state : State
    }


defaultModel : Session -> Model
defaultModel session =
    { session = session, state = Lobby }


init : Session -> ( Model, Cmd Msg )
init session =
    case session of
        Session.Client _ _ ->
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


displayPlayerDeck : Game.Model -> Game.Player -> Html Msg
displayPlayerDeck game player =
    div [ class "player-deck" ]
        [ text player.name
        , div [ class "cards" ]
            (List.map (\card -> Game.CardView.view [ onClick (PlayCard player card), disabled (not (Game.canPlayCard card game)) ] { size = "150px", flipped = True } card) player.hand)
        ]


displayDrawStack : Game.Draw -> Game.Player -> Html Msg
displayDrawStack stack player =
    div [ class "draw-stack", onClick (DrawCard player) ]
        [ div [ class "cards" ]
            (List.map (\card -> Game.CardView.view [] { size = "150px", flipped = False } card) stack)
        ]



-- UPDATE


type Msg
    = StartGame
    | NewGame Game.Model
    | BackLobby
    | PlayCard Game.Player Card
    | PlayPlayableCard Game.Player PlayableCard
    | DrawCard Game.Player
    | NextTurn


playCard : Game.Player -> PlayableCard -> Game.Model -> Game.Model
playCard player playableCard game =
    Game.playerPlayCard player playableCard game
        |> Tuple.first
        |> Game.nextTurn
        |> Game.applyCardEffect (Game.Card.getCard playableCard)


playOrChoiceCard : Game.Player -> Card -> GameModel -> GameModel
playOrChoiceCard player card game =
    case card of
        WildCard _ ->
            { game | state = Choice player card }

        _ ->
            { game | model = game.model |> playCard player (StandardCard card) }


updateGame : Msg -> GameModel -> ( GameModel, Cmd Msg )
updateGame msg game =
    case msg of
        PlayCard player card ->
            ( playOrChoiceCard player card game, Cmd.none )

        PlayPlayableCard player playableCard ->
            ( { game | model = game.model |> playCard player playableCard, state = Idle }, Cmd.none )

        DrawCard player ->
            ( { game | model = game.model |> Game.drawCard 1 player |> Game.nextTurn }, Cmd.none )

        NextTurn ->
            ( { game | model = game.model |> Game.nextTurn }, Cmd.none )

        _ ->
            ( game, Cmd.none )


initGame : Game.Model
initGame =
    Game.emptyGame


updateNoGame : Msg -> Model -> ( Model, Cmd Msg )
updateNoGame msg model =
    case msg of
        StartGame ->
            ( model
            , Game.newGame NewGame
            )

        NewGame game ->
            ( { model
                | state =
                    Game
                        { model =
                            game
                                |> Game.getFirstCard
                                |> Game.addPlayer ( "1", "Player 1" )
                                |> Game.addPlayer ( "2", "Player 2" )
                                |> Game.addPlayer ( "3", "Player 3" )
                                |> Game.addPlayer ( "4", "Player 4" )
                        , state = Idle
                        }
              }
            , Cmd.none
            )

        BackLobby ->
            ( model, Route.replaceUrl (Session.navKey model.session) Route.Lobby )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.state of
        Lobby ->
            updateNoGame msg model

        Game game ->
            let
                ( newGame, cmd ) =
                    updateGame msg game
            in
            ( { model | state = Game newGame }, cmd )



-- VIEW HELPER FUNCTIONS


viewGame : Game.Model -> Html Msg
viewGame game =
    case Game.getCurrentPlayer game of
        ( Just currentPlayer, otherPlayers ) ->
            div [ class "game" ]
                [ div [ class "topbar" ] (List.map printPlayer otherPlayers)
                , displayPlayerDeck game currentPlayer
                , div [ class "center" ]
                    [ displayDrawStack (List.take 3 game.drawStack) currentPlayer
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


viewChoice : Game.Player -> Card -> Game.Model -> Html Msg
viewChoice player card _ =
    div [ class "choice-modal" ]
        [ div [ class "content" ]
            [ div [ class "title" ] [ text "Choose a color" ]
            , ul []
                [ li
                    [ class "active"
                    ]
                    [ button [ onClick (PlayPlayableCard player (ChoiceCard card Game.Color.Red)) ] [ text "Red" ] ]
                , li
                    [ class "active"
                    ]
                    [ button [ onClick (PlayPlayableCard player (ChoiceCard card Game.Color.Blue)) ] [ text "Blue" ] ]
                , li
                    [ class "active"
                    ]
                    [ button [ onClick (PlayPlayableCard player (ChoiceCard card Game.Color.Green)) ] [ text "Green" ] ]
                , li
                    [ class "active"
                    ]
                    [ button [ onClick (PlayPlayableCard player (ChoiceCard card Game.Color.Yellow)) ] [ text "Yellow" ] ]
                ]
            ]
        ]



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Room"
    , content =
        case model.state of
            Lobby ->
                viewLobby model

            Game game ->
                case game.state of
                    Idle ->
                        viewGame game.model

                    Choice player card ->
                        viewChoice player card game.model
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
