module Main exposing (..)

import Browser
import CardGame exposing (..)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MAIN


type Msg
    = Start


main : Program () CardGame.Game Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


init : () -> ( CardGame.Game, Cmd Msg )
init _ =
    ( CardGame.initGame, Cmd.none )


view : CardGame.Game -> Html Msg
view gameView =
    div []
        [ div []
            [ button [ onClick Start ] [ text "Start" ] ]
        , div []
            [ text (toString gameView) ]
        ]


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        Start ->
            ( game, Cmd.none )


subscriptions : Game -> Sub Msg
subscriptions _ =
    Sub.none
