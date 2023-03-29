port module Lobby exposing (..)

import Html exposing (Html, button, div, img, input, li, text, ul)
import Html.Attributes exposing (class, placeholder, src, value)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Route
import Session exposing (RoomData(..), Session)



-- PORTS


port joinRoom : String -> Cmd msg


port createRoom : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
    { session : Session
    , isHost : Bool
    , code : Maybe String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , isHost = False
      , code = Nothing
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Lobby"
    , content =
        -- div []
        --     [ button [ onClick HostGame ] [ text "Host Game" ]
        --     , button [ onClick JoinGame ] [ text "Join Game" ]
        --     , button [ onClick StartGame ] [ text "Start Game" ]
        --     , if model.isHost then
        --         div [] [ text ("Code: " ++ Maybe.withDefault "" model.code) ]
        --       else
        --         input [ placeholder "Code", onInput CodeInput ] []
        --     ]
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
                    div [] [ text ("Code: " ++ Maybe.withDefault "" model.code) ]

                  else
                    input [ placeholder "Code", onInput CodeInput, value (Maybe.withDefault "" model.code) ] []
                ]
            ]
    }



-- UPDATE


type Msg
    = HostGame
    | JoinGame
    | StartGame
    | CodeInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HostGame ->
            ( { model | isHost = True }, Cmd.none )

        JoinGame ->
            ( { model | isHost = False }, Cmd.none )

        StartGame ->
            case model.code of
                Just code ->
                    if model.isHost then
                        ( model, Route.replaceUrl (Session.navKey model.session) (Route.Room code) )

                    else
                        ( model, Cmd.batch [ joinRoom code, Route.replaceUrl (Session.navKey model.session) (Route.Room code) ] )

                Nothing ->
                    ( model, Cmd.none )

        CodeInput code ->
            ( { model | code = Just code }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ createRoom CodeInput
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    Session.setRoomData (RoomData{ isHost = model.isHost, code = Maybe.withDefault "" model.code }) model.session
