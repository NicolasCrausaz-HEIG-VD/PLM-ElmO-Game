port module Pages.Lobby exposing (..)

import Game.Core
import Html exposing (Html, button, code, div, img, input, li, text, ul)
import Html.Attributes exposing (class, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Route
import Session exposing (Session)
import Utils exposing (Code)



-- PORTS


port joinRoom : Code -> Cmd msg


port joinedRoom : (Code -> msg) -> Sub msg


port requestRoomCode : () -> Cmd msg


port createRoom : (Code -> msg) -> Sub msg



-- MODEL


type alias Model =
    { session : Session
    , isHost : Bool
    , code : Maybe Code
    , hostCode : Maybe Code
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , isHost = False
      , code = Nothing
      , hostCode = Nothing
      }
    , Cmd.batch [ requestRoomCode () ]
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Lobby"
    , content =
        div [ class "lobby-menu" ]
            [ div [ class "content" ]
                [ img [ src "/logo.svg" ] []
                , div [ class "title" ] [ text "ElmO" ]
                , ul []
                    [ li
                        [ class "active"
                        ]
                        [ button [ onClick HostGame ] [ text "Host Game" ] ]
                    , li []
                        [ button [ onClick JoinGame ] [ text "Join Game" ] ]
                    , li []
                        [ button [ onClick StartGame ] [ text "Start Game" ] ]
                    ]
                , if model.isHost then
                    div [] [ text ("Code: " ++ Maybe.withDefault "" model.hostCode) ]

                  else
                    input [ placeholder "Code", onInput CodeInput, value (Maybe.withDefault "" model.code) ] []
                ]
            ]
    }



-- UPDATE


type Msg
    = HostGame
    | JoinGame
    | StartHostGame Code Game.Core.Model
    | StartClientGame Code
    | StartGame
    | CodeInput Code
    | SetHostCode Code


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HostGame ->
            ( { model | isHost = True }, Cmd.none )

        JoinGame ->
            ( { model | isHost = False }, Cmd.none )

        StartGame ->
            case getRoomCode model of
                Just code ->
                    if model.isHost then
                        ( model, Game.Core.newGame (StartHostGame code) )

                    else
                        ( model, joinRoom code )

                Nothing ->
                    ( model, Cmd.none )

        CodeInput code ->
            ( { model | code = Just (String.left 4 (String.toUpper code)) }, Cmd.none )

        SetHostCode code ->
            ( { model | hostCode = Just code }, Cmd.none )

        StartClientGame code ->
            ( { model | session = model.session |> Session.update (Session.Client { code = code }) }, Route.replaceUrl model.session.key (Route.Party code) )

        StartHostGame code gameModel ->
            ( { model | session = model.session |> Session.update (Session.Host gameModel { code = code }) }, Route.replaceUrl model.session.key (Route.Party code) )



-- HELPERS


getRoomCode : Model -> Maybe String
getRoomCode model =
    if model.isHost then
        model.hostCode

    else
        model.code



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ createRoom SetHostCode
        , joinedRoom StartClientGame
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
