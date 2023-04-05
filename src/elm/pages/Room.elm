port module Pages.Room exposing (..)

import Game.Card exposing (Card, toString)
import Game.Game as Game
import Html exposing (Html, button, div, li, p, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Route
import Session exposing (RoomData, Session)



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


printCards : List Card -> Html msg
printCards cards =
    ul []
        (List.map (\card -> li [] [ text (toString card) ]) cards)


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
                div []
                    [ div []
                        [ p [] [ text "Game running" ]
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
            ( { model | game = Just Game.initGame }, Cmd.none )

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
