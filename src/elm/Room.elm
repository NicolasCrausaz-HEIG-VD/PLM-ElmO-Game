module Room exposing (..)

import Browser exposing (Document)
import Game.Card exposing (Card)
import Game.Game as Game
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , code : String
    , game : Maybe Game.State
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , code = "1234"
      , game = Nothing
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Room " ++ model.code
    , content =
        case model.game of
            Nothing ->
                div []
                    [ button [ onClick StartGame ] [ text "Start Game" ]
                    ]

            Just game ->
                div []
                    [ div [] [ text "Game is running" ] ]
    }



-- UPDATE


type Msg
    = StartGame
    | CardClicked Card


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( { model | game = Just Game.initGame }, Cmd.none )

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
