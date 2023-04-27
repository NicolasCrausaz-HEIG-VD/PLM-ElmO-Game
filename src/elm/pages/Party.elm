port module Pages.Party exposing (..)

import Dict exposing (Dict)
import Game.Card
import Game.CardView
import Game.Client
import Game.Core
import Game.Host
import Game.JSON
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Session exposing (Session)
import Html exposing (button)
import Html exposing (img)
import Html exposing (li)
import Html.Attributes exposing (src)
import Html.Attributes exposing (style)
import Game.Color



-- MODEL


type GameState
    = Playing
    | SelectColor Game.Card.Card


type alias Model =
    { session : Session
    , state : GameState
    , host : Maybe Game.Host.Game
    , game : Maybe Game.Client.Model
    }


init : Session -> ( Model, Cmd Msg )
init session =
    case session of
        _ ->
            ( { session = session, game = Nothing, host = Nothing, state = Playing }, Game.Core.newGame (HostMsg << StartHost) )



-- PORTS


port incomingData : (E.Value -> msg) -> Sub msg


port outgoingData : E.Value -> Cmd msg


port incomingAction : (E.Value -> msg) -> Sub msg


port outgoingAction : E.Value -> Cmd msg



--- UPDATE


type ClientMsg
    = PlayCard Game.Card.PlayableCard
    | DrawCard
    | ClickCard Game.Card.Card
    | SetState GameState
    | IncomingData E.Value
    | SendAction Game.Host.Action


type HostMsg
    = IncomingAction E.Value
    | SendData  Game.Host.Game
    | StartHost Game.Host.Game


type Msg
    = NoOp
    | HostMsg HostMsg
    | ClientMsg ClientMsg


clientUpdate : ClientMsg -> Model -> ( Model, Cmd Msg )
clientUpdate msg model =
    case ( msg, model.game ) of
        ( DrawCard, Just game ) ->
            clientUpdate (SendAction (Game.Host.DrawCard game.localPlayer.uuid)) ( { model | state = Playing })
        
        ( PlayCard card, Just game ) ->
            clientUpdate (SendAction (Game.Host.PlayCard game.localPlayer.uuid card)) ( { model | state = Playing } ) 
        
        ( ClickCard card, Just _ ) ->
            case card of
                Game.Card.WildCard _ ->
                    ( { model | state = SelectColor card }, Cmd.none )

                _ ->
                    clientUpdate (PlayCard (Game.Card.StandardCard card)) model
        ( SetState state, _ ) ->
            ( { model | state = state }, Cmd.none )
        ( SendAction action, _ ) ->
            ( model, outgoingAction (Game.JSON.encodeAction action))

        ( IncomingData data, _ ) ->
            case D.decodeValue Game.JSON.decodeModel data of
                Ok newGame ->
                    ( { model | game = Just newGame }, Cmd.none )

                Err err ->
                    ( model, Debug.log ("Error decoding incoming data: " ++ Debug.toString err) Cmd.none )

        _ ->
            ( model, Cmd.none )


hostUpdate : HostMsg -> Model -> ( Model, Cmd Msg )
hostUpdate msg model =
    case ( msg, model.host ) of
        ( IncomingAction data, Just host ) ->
            case D.decodeValue Game.JSON.decodeAction data of
                Ok action ->
                    case Game.Host.onAction action host of
                        (newHost, True) ->
                            hostUpdate (SendData newHost) model
                        (newHost, _) -> 
                            ( { model | host = Just newHost }, Cmd.none )

                Err err ->
                    ( model, Debug.log ("Error decoding incoming action: " ++ Debug.toString err) Cmd.none )

        ( SendData host, Just _ ) ->
            case Game.Host.toClient host "uuid1" of
                Just clientGame ->
                    ( { model | host = Just host }, outgoingData (Game.JSON.encodeModel clientGame) )

                Nothing ->
                    ( model, Debug.log "Tried to send data but no client game was available" Cmd.none )

        ( StartHost host, _ ) ->
            let
                game =
                    host
                        |> Game.Core.addPlayer ( "uuid1", "Player 1" )
                        |> Game.Core.addPlayer ( "uuid2", "Player 2" )
                        |> Game.Core.addPlayer ( "uuid3", "Player 3" )
                        |> Game.Core.addPlayer ( "uuid4", "Player 4" )
            in
            ( { model | host = Just game, game = Game.Host.toClient game "uuid1" }, Cmd.none )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HostMsg hostMsg ->
            hostUpdate hostMsg model
        ClientMsg clientMsg ->
            clientUpdate clientMsg model
        _ ->
            ( model, Cmd.none )



-- VIEW


displayDistantPlayer : Game.Client.DistantPlayer -> Html ClientMsg
displayDistantPlayer player =
    div [ class "player" ]
        [ text player.name
        , div [ class "cards" ]
            (List.repeat player.cards (Game.CardView.emptyView [ class "card" ]))
        ]


displayPlayerDeck : Game.Client.LocalPlayer -> Game.Client.Model -> Html ClientMsg
displayPlayerDeck player model =
    div [ class "player-deck" ]
        [ text player.name
        , div [ class "cards" ]
            (List.map (\card -> Game.CardView.cardView [ class "card", onClick (ClickCard card), disabled (not (Game.Card.canPlayCard card ( model.activeCard, model.activeColor ))) ] card) player.hand)
        ]


displayDrawStack : Int -> Html ClientMsg
displayDrawStack stack =
    div [ class "draw-stack" ]
        [ div [ class "cards" ]
            (List.repeat (min 3 stack) (Game.CardView.emptyView [ class "card", onClick DrawCard ]))
        ]




viewGame : Game.Client.Model -> Html ClientMsg
viewGame model =
    div [ class "game" ]
        [ div [ class "topbar" ] (List.map displayDistantPlayer (Dict.values model.distantPlayers))
        , displayPlayerDeck model.localPlayer model
        , div [ class "center" ]
            [ displayDrawStack model.drawStack
            , div [ class "active-card" ]
                [ case model.activeCard of
                    Just card ->
                        Game.CardView.cardView [ class "card" ] card

                    Nothing ->
                        div [] []
                ]
            ]
        ]

viewChoice : Game.Card.Card -> Html ClientMsg
viewChoice card =
    div [ class "choice-modal" ]
        [ div [ class "content" ]
            [ div [ class "title" ] [ text "Choose a color" ]
            , button [ onClick (SetState Playing) ] [ text "Cancel" ]
            , div []
                [ li
                    []
                    [ button [ class "card active", onClick (PlayCard (Game.Card.ChoiceCard card Game.Color.Red)) ]
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
                    [ button [ class "card active", onClick (PlayCard (Game.Card.ChoiceCard card Game.Color.Blue)) ]
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
                    [ button [ class "card active", onClick (PlayCard (Game.Card.ChoiceCard card Game.Color.Green)) ]
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
                    [ button [ class "card active", onClick (PlayCard (Game.Card.ChoiceCard card Game.Color.Yellow)) ]
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


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Party Elmo"
    , content =
        case (model.game, model.state) of
            ( Just game, Playing ) ->
                viewGame game |> Html.map ClientMsg
            ( Just _, SelectColor card ) ->
                viewChoice card |> Html.map ClientMsg
            ( Nothing, _ ) ->
                div [] [ text "Loading..." ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ incomingData (ClientMsg << IncomingData)
        , incomingAction (HostMsg << IncomingAction)
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
