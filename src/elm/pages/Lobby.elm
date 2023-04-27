module Pages.Lobby exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Game.Core
import Html exposing (Html, button, code, div, img, input, text)
import Html.Attributes exposing (class, classList, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Network
import Route
import Session exposing (Session)
import Utils exposing (Code, UUID)



-- MODEL


type Mode
    = Host
    | Client


type alias Model =
    { session : Session
    , mode : Mode
    , username : String
    , code : Maybe Code
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , mode = Client
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
                    [ button [ onClick (ToggleMode Host), classList [ ( "active", model.mode == Host ) ] ] [ text "Host Game" ]
                    , button [ onClick (ToggleMode Client), classList [ ( "active", model.mode == Client ) ] ] [ text "Join Game" ]
                    ]
                , div [ class "container" ]
                    (case model.mode of
                        Host ->
                            [ text "Ready to host a game!"
                            , input [ placeholder "Username", onInput SetUsername, value model.username ] []
                            , button [ onClick StartGame ] [ text "Start Game" ]
                            ]

                        Client ->
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
    = ToggleMode Mode
    | StartHostGame Code Game.Core.Model
    | StartClientGame ( Code, UUID )
    | StartGame
    | SetCode Code
    | SetUsername String
    | RequestNewGame Code


validateUsername : String -> ( String, Bool )
validateUsername username =
    ( username, String.length username > 0 )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleMode mode ->
            ( { model | mode = mode }, Cmd.none )

        StartGame ->
            case ( model.code, model.username |> validateUsername |> Tuple.second, model.mode ) of
                ( _, True, Host ) ->
                    ( model, Network.createRoom () )

                ( Just code, True, Client ) ->
                    ( model, Network.joinRoom code )

                _ ->
                    ( model, Cmd.none )

        SetCode code ->
            ( { model | code = Just code }, Cmd.none )

        SetUsername username ->
            ( { model | username = username }, Cmd.none )

        StartClientGame ( code, playerUUID ) ->
            ( { model | session = model.session |> Session.update (Session.Client { code = code, playerUUID = playerUUID, username = model.username }) }, Route.replaceUrl model.session.key (Route.Room code) )

        StartHostGame code gameModel ->
            ( { model | session = model.session |> Session.update (Session.Host gameModel { code = code, playerUUID = code, username = model.username }) }, Route.replaceUrl model.session.key (Route.Room code) )

        RequestNewGame code ->
            ( model, Game.Core.newGame (StartHostGame code) )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Network.joinedRoom StartClientGame
        , Network.createdRoom RequestNewGame
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
