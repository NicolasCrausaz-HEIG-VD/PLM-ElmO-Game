module Session exposing (..)

import Browser.Navigation as Nav
import Game.Core
import Utils exposing (RoomData)


type SessionType
    = NotConnected
    | Host Game.Core.Model RoomData
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
