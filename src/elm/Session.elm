module Session exposing (..)

import Browser.Navigation as Nav


type RoomData
    = RoomData
        { isHost : Bool
        , code : String
        }


type Session
    = NotConnected Nav.Key
    | Connected Nav.Key RoomData


create : Nav.Key -> Session
create =
    NotConnected


navKey : Session -> Nav.Key
navKey session =
    case session of
        NotConnected key ->
            key

        Connected key _ ->
            key


setRoomData : RoomData -> Session -> Session
setRoomData data session =
    Connected (navKey session) data
