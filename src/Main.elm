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
    let
        game =
            CardGame.initGame
                |> CardGame.addPlayer "Player 1"
                |> CardGame.addPlayer "Player 2"
    in
    ( game, Cmd.none )


view : CardGame.Game -> Html Msg
view game =
    div []
        [ button [ onClick Start ] [ text "Start" ]
        , div []
            (List.map printPlayer game.players)
        , div []
            [ printCards game.drawStack
            ]
        ]


printPlayer : Player -> Html Msg
printPlayer player =
    div []
        [ text player.name
        , div []
            [ printCards player.hand
            ]
        ]


printCards : List Card -> Html Msg
printCards cards =
    ul []
        (List.map (\card -> li [] [ text (toString card) ]) cards)


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        Start ->
            ( game, Cmd.none )


subscriptions : Game -> Sub Msg
subscriptions _ =
    Sub.none
