port module Main exposing (..)

import Browser
import CardGame exposing (..)
import Debug exposing (toString)
import Html exposing (..)
import Html.Events exposing (..)
import Random

-- PORTS

port sendMsg : String -> Cmd msg

port handleMsg : (String -> msg) -> Sub msg

-- MAIN

type Msg = Start Int | Next


main : Program () CardGame.Game Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


init : () -> ( CardGame.Game, Cmd Msg )
init _ = ( CardGame.initGame, Cmd.none )


view : CardGame.Game -> Html Msg
view game =
    div []
        [ button [ onClick (Start 1) ] [ text "Start" ]
        , button [ onClick Next ] [ text "Next" ]
        , div []
            (List.map printPlayer game.players)
        , div []
            [
                p [] [ text "Current player:", text (toString (CardGame.getCurrentPlayer game)) ]
                ,p [] [ text "Current card:", text (toString game.activeCard) ]
                ,p [] [ text "Current color:", text (toString game.activeColor) ]
                ,printCards game.drawStack
            ]
        ]

update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        Start seed -> ( (CardGame.shuffleGame game (Random.initialSeed seed))
            |> CardGame.addPlayer("Player 1")
            |> CardGame.addPlayer("Player 2")
            |> CardGame.addPlayer("Player 3")
            |> CardGame.getFirstCard
            , Cmd.none )
        Next -> ( game |> CardGame.nextTurn, sendMsg "next" )




subscriptions : Game -> Sub Msg
subscriptions _ =
    -- TODO: handle messages from the server
    handleMsg (\msg -> case msg of
        "next" -> Next
        _ -> Next
    )


-- HELPERS


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
