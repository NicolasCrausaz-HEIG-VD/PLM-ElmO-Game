port module Network exposing (createRoom, createdRoom, joinRoom, joinedRoom)

import Utils exposing (Code, UUID)



-- Port


port joinRoom : Code -> Cmd msg


port joinedRoom : (( Code, UUID ) -> msg) -> Sub msg


port createRoom : () -> Cmd msg


port createdRoom : (Code -> msg) -> Sub msg
