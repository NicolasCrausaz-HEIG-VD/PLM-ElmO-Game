module Session exposing (..)

import Browser.Navigation as Nav
import Game.Core
import Utils exposing (RoomData)


type alias HostConfig =
    { game : Game.Core.Model
    , nbAI : Int
    }


type SessionType
    = NotConnected
    | Host HostConfig RoomData
    | Client RoomData


type alias Session =
    { key : Nav.Key
    , session : SessionType
    }


create : Nav.Key -> Session
create key =
    { key = key
    , session = NotConnected
    }


update : SessionType -> Session -> Session
update sessionType session =
    { session | session = sessionType }


getKey : Session -> Nav.Key
getKey session =
    session.key
