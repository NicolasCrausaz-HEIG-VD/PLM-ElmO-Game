module Session exposing (..)

import Browser.Navigation as Nav


type Session
    = Connected Nav.Key


create : Nav.Key -> Session
create =
    Connected


navKey : Session -> Nav.Key
navKey session =
    case session of
        Connected key ->
            key
