port module Pages.Lobby exposing (..)

import Html exposing (Html, button, div, img, input, li, text, ul)
import Html.Attributes exposing (class, placeholder, src, value)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Route
import Session exposing (RoomData(..), Session)



-- PORTS


port joinRoom : String -> Cmd msg

port joinedRoom : (String -> msg) -> Sub msg

port requestRoomCode : () -> Cmd msg

port createRoom : (String -> msg) -> Sub msg


-- MODEL

type alias Model =
    { session : Session
    , isHost : Bool
    , code : Maybe String
    , hostCode : Maybe String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , isHost = False
      , code = Nothing
      , hostCode = Nothing
      }
    ,
     Cmd.batch [ requestRoomCode () ]
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
    | StartingGame String
    | StartGame
    | CodeInput String
    | SetHostCode String


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
                        update (StartingGame code) model
                    else
                        ( model, joinRoom code )

                Nothing ->
                    ( model, Cmd.none )

        CodeInput code ->
            ( { model | code = Just (String.left 4 (String.toUpper code)) }, Cmd.none )
        SetHostCode code ->
            ( { model | hostCode = Just code }, Cmd.none )
        StartingGame code ->
            ( model, Route.replaceUrl (Session.navKey model.session) (Route.Room code) )

-- HELPERS
getRoomCode : Model -> Maybe String
getRoomCode model =
    if model.isHost then
        model.hostCode
    else
        model.code


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ createRoom SetHostCode
        , joinedRoom StartingGame
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    Session.setRoomData (RoomData{ isHost = model.isHost, code = Maybe.withDefault "" (getRoomCode model) }) model.session