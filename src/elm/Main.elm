port module Main exposing (..)

import Browser
import Card exposing (Card)
import CardView
import Debug exposing (toString)
import Game exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (..)
import Random



-- PORTS


port sendMsg : String -> Cmd msg


port handleMsg : (String -> msg) -> Sub msg


port joinRoom : String -> Cmd msg


port createRoom : (String -> msg) -> Sub msg



-- MAIN


type Msg
    = Start Int
    | Next
    | EditRoomId String
    | JoinRoom
    | CardClicked Card


type alias Model =
    { game : Game.State
    , roomId : Maybe String
    }


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game = Game.initGame
      , roomId = Nothing
      }
    , Cmd.none
    )


view : Model -> Html Msg
view { game, roomId } =
    div []
        [ button [ onClick (Start 1) ] [ text "Start" ]
        , button [ onClick Next ] [ text "Next" ]
        , input [ type_ "text", placeholder "Room ID", value (Maybe.withDefault "" roomId), onInput EditRoomId ] []
        , button [ onClick JoinRoom ] [ text "Join room" ]
        , div []
            (List.map printPlayer game.players)
        , div []
            [ p [] [ text "Current player:", text (toString (Game.getCurrentPlayer game)) ]
            , p [] [ text "Current card:", text (toString game.activeCard) ]
            , p [] [ text "Current color:", text (toString game.activeColor) ]
            , printCards game.drawStack
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Start seed, { game } ) ->
            let
                shuffledGame =
                    game
                        |> Game.shuffleGame (Random.initialSeed seed)
                        |> Game.addPlayer "Player 1"
                        |> Game.addPlayer "Player 2"
                        |> Game.addPlayer "Player 3"
                        |> Game.getFirstCard
            in
            ( { model | game = shuffledGame }, Cmd.none )

        ( Next, { game } ) ->
            ( { model | game = game |> Game.nextTurn }, sendMsg "next" )

        ( CardClicked card, _ ) ->
            let
                _ =
                    Debug.log "Card clicked" card
            in
            ( model, Cmd.none )

        ( EditRoomId roomId, _ ) ->
            ( { model | roomId = Just roomId }, Cmd.none )

        ( JoinRoom, { roomId } ) ->
            ( model, Maybe.map joinRoom roomId |> Maybe.withDefault Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ handleMsg
            (\msg ->
                case msg of
                    "next" ->
                        Next

                    _ ->
                        Next
            )
        , createRoom (\roomId -> EditRoomId roomId)
        ]



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
    div [ class "cards" ]
        (List.map (\card -> CardView.view [ onClick (CardClicked card) ] { size = "100px", flipped = True } card) cards)
