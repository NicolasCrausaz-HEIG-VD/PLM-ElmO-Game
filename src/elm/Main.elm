port module Main exposing (..)

import Browser
import Card exposing (Card)
import CardView
import Game exposing (..)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import Random

-- PORTS

port sendMsg : String -> Cmd msg

port handleMsg : (String -> msg) -> Sub msg

-- MAIN

type Msg = Start Int | Next | CardClicked Card


main : Program () Game.State Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


init : () -> ( Game.State, Cmd Msg )
init _ = ( Game.initGame, Cmd.none )


view : Game.State -> Html Msg
view game =
    div []
        [ button [ onClick (Start 1) ] [ text "Start" ]
        , button [ onClick Next ] [ text "Next" ]
        , div []
            (List.map printPlayer game.players)
        , div []
            [
                p [] [ text "Current player:", text (toString (Game.getCurrentPlayer game)) ]
                ,p [] [ text "Current card:", text (toString game.activeCard) ]
                ,p [] [ text "Current color:", text (toString game.activeColor) ]
                ,printCards game.drawStack
            ]
        ]

update : Msg -> State -> ( State, Cmd Msg )
update msg game =
    case msg of
        Start seed -> ( (Game.shuffleGame game (Random.initialSeed seed))
            |> Game.addPlayer("Player 1")
            |> Game.addPlayer("Player 2")
            |> Game.addPlayer("Player 3")
            |> Game.getFirstCard
            , Cmd.none )
        Next -> ( game |> Game.nextTurn, sendMsg "next" )
        CardClicked card ->
            let
                _ = Debug.log "Clicked card" card
            in
                ( game, Cmd.none )




subscriptions : State -> Sub Msg
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
    div [class "cards"]
    (List.map (\card -> (CardView.view [onClick (CardClicked card)] {size = "100px", flipped = True} card)) cards)
