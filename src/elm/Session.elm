module Session exposing (..)

import Browser.Navigation as Nav
import Game.Core


type RoomData
    = RoomData
        { isHost : Bool
        , code : String
        }


type Session
    = NotConnected Nav.Key
    | Host Nav.Key RoomData Game.Core.Model
    | Client Nav.Key RoomData


create : Nav.Key -> Session
create =
    NotConnected


navKey : Session -> Nav.Key
navKey session =
    case session of
        NotConnected key ->
            key

        Host key _ _ ->
            key

        Client key _ ->
            key


setRoomData : RoomData -> Session -> Session
setRoomData data session =
    Client (navKey session) data
