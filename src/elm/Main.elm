module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Pages.Lobby as Lobby
import Pages.Room as Room
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)


type Model
    = Redirect Session
    | Room Room.Model
    | Lobby Lobby.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.create navKey))



-- VIEW


view : Model -> Document Msg
view model =
    case model of
        Room room ->
            viewWith RoomMsg (Room.view room)

        Lobby lobby ->
            viewWith LobbyMsg (Lobby.view lobby)

        Redirect _ ->
            { title = "Redirecting...", body = [] }


viewWith : (subMsg -> Msg) -> { title : String, content : Html subMsg } -> Document Msg
viewWith toMsg { title, content } =
    { title = title
    , body = [ content |> Html.map toMsg ]
    }



-- UPDATE


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | RoomMsg Room.Msg
    | LobbyMsg Lobby.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url) )

                Browser.External _ ->
                    ( model, Cmd.none )

        ( RoomMsg subMsg, Room room ) ->
            Room.update subMsg room
                |> updateWith Room RoomMsg model

        ( LobbyMsg subMsg, Lobby lobby ) ->
            Lobby.update subMsg lobby
                |> updateWith Lobby LobbyMsg model

        _ ->
            ( model, Cmd.none )



-- helpers for sub-models


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg _ ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


toSession : Model -> Session
toSession model =
    case model of
        Room room ->
            Room.toSession room

        Lobby lobby ->
            Lobby.toSession lobby

        Redirect session ->
            session


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    let
        session =
            toSession model
    in
    case route of
        Just (Route.Room _) ->
            Room.init session
                |> updateWith Room RoomMsg model

        Just Route.Lobby ->
            Lobby.init session
                |> updateWith Lobby LobbyMsg model

        Nothing ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Room room ->
            Sub.map RoomMsg (Room.subscriptions room)

        Lobby lobby ->
            Sub.map LobbyMsg (Lobby.subscriptions lobby)

        _ ->
            Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
