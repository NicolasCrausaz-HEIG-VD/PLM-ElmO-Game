port module Pages.Lobby exposing (..)

import Game.Core
import Html exposing (Html, button, code, div, img, input, li, text, ul)
import Html.Attributes exposing (class, classList, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Route
import Session exposing (Session)
import Utils exposing (Code, UUID)



-- PORTS


port joinRoom : ( Code, String ) -> Cmd msg


port joinedRoom : (( Code, UUID ) -> msg) -> Sub msg


port requestRoomCode : String -> Cmd msg


port createRoom : (Code -> msg) -> Sub msg



-- MODEL


type alias Model =
    { session : Session
    , isHost : Bool
    , username : String
    , code : Maybe Code
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , isHost = False
      , username = ""
      , code = Nothing
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Lobby"
    , content =
        div [ class "lobby-menu" ]
            [ div [ class "content" ]
                [ img [ src "/logo.svg" ] []
                , img [ src "/logo_text.svg", class "title" ] []
                , div [ class "tabs" ]
                    [ button [ onClick HostGame, classList [ ( "active", model.isHost ) ] ] [ text "Host Game" ]
                    , button [ onClick JoinGame, classList [ ( "active", not model.isHost ) ] ] [ text "Join Game" ]
                    ]
                , div [ class "container" ]
                    (if model.isHost then
                        [ text "Ready to host a game!"
                        , input [ placeholder "Username", onInput SetUsername, value model.username ] []
                        , button [ onClick StartGame ] [ text "Start Game" ]
                        ]

                     else
                        [ input [ placeholder "Code", onInput SetCode, value (Maybe.withDefault "" model.code) ] []
                        , input [ placeholder "Username", onInput SetUsername, value model.username ] []
                        , button [ onClick StartGame ] [ text "Join Game" ]
                        ]
                    )
                ]
            ]
    }



-- UPDATE


type Msg
    = HostGame
    | JoinGame
    | StartHostGame Code Game.Core.Model
    | StartClientGame ( Code, UUID )
    | StartGame
    | SetCode Code
    | SetUsername String
    | RequestNewGame Code


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HostGame ->
            ( { model | isHost = True }, Cmd.none )

        JoinGame ->
            ( { model | isHost = False }, Cmd.none )

        StartGame ->
            case ( model.code, model.username, model.isHost ) of
                ( _, username, True ) ->
                    ( model, requestRoomCode username )

                ( Just code, username, False ) ->
                    ( model, joinRoom ( code, username ) )

                _ ->
                    ( model, Cmd.none )

        SetCode code ->
            ( { model | code = Just code }, Cmd.none )

        SetUsername username ->
            ( { model | username = username }, Cmd.none )

        StartClientGame ( code, playerUUID ) ->
            ( { model | session = model.session |> Session.update (Session.Client { code = code, playerUUID = playerUUID }) }, Route.replaceUrl model.session.key (Route.Party code) )

        StartHostGame code gameModel ->
            ( { model | session = model.session |> Session.update (Session.Host gameModel { code = code, playerUUID = code }) }, Route.replaceUrl model.session.key (Route.Party code) )

        RequestNewGame code ->
            ( model, Game.Core.newGame (StartHostGame code) )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ joinedRoom StartClientGame
        , createRoom RequestNewGame
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
