port module Network exposing (createRoom, createdRoom, joinRoom, joinedRoom, lostConnection)

import Utils exposing (Code, UUID)



-- Port


port joinRoom : Code -> Cmd msg


port joinedRoom : (( Code, UUID, Bool ) -> msg) -> Sub msg


port lostConnection : (Code -> msg) -> Sub msg


port createRoom : () -> Cmd msg


port createdRoom : (Code -> msg) -> Sub msg
