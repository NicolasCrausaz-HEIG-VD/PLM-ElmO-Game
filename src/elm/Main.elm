module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Pages.Lobby as Lobby
import Pages.Room as Room
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)
import Utils exposing (updateWith, viewWith)


type Model
    = Redirect Session
    | Lobby Lobby.Model
    | Room Room.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.create navKey))



-- VIEW


view : Model -> Document Msg
view model =
    case model of
        Lobby lobby ->
            Lobby.view lobby |> viewWith LobbyMsg

        Room room ->
            Room.view room |> viewWith RoomMsg

        Redirect _ ->
            { title = "Redirecting...", body = [] }



-- UPDATE


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | LobbyMsg Lobby.Msg
    | RoomMsg Room.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (toSession model).key (Url.toString url) )

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


toSession : Model -> Session
toSession model =
    case model of
        Lobby lobby ->
            Lobby.toSession lobby

        Room room ->
            Room.toSession room

        Redirect session ->
            session


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    let
        session =
            toSession model
    in
    case route of
        Just Route.Lobby ->
            Lobby.init session
                |> updateWith Lobby LobbyMsg model

        Just (Route.Room code) ->
            Room.init code session
                |> updateWith Room RoomMsg model

        Nothing ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Lobby lobby ->
            Sub.map LobbyMsg (Lobby.subscriptions lobby)

        Room room ->
            Sub.map RoomMsg (Room.subscriptions room)

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
