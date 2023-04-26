module Game.Client exposing (..)

import Dict exposing (Dict)
import Game.Card exposing (Card, PlayableCard(..))
import Game.Color exposing (Color)
import Game.Core exposing (Hand, Player, UUID, allCards)


type alias DistantPlayer =
    { name : String
    , uuid : UUID
    , cards : Int
    }


type alias LocalPlayer =
    { name : String
    , uuid : UUID
    , hand : Hand
    }


type alias Model =
    { distantPlayers : Dict UUID DistantPlayer
    , localPlayer : LocalPlayer
    , currentPlayer : UUID
    , drawStack : Int
    , activeCard : Maybe Card
    , activeColor : Maybe Color
    }


toDistantPlayer : Player -> DistantPlayer
toDistantPlayer player =
    { name = player.name
    , uuid = player.uuid
    , cards = List.length player.hand
    }


toLocalPlayer : Player -> LocalPlayer
toLocalPlayer player =
    { name = player.name
    , uuid = player.uuid
    , hand = player.hand
    }


defaultGameModel : Model
defaultGameModel =
    { distantPlayers =
        Dict.fromList
            [ ( "uuid2", { name = "Nicolas", uuid = "uuid2", cards = 4 } )
            , ( "uuid3", { name = "Julien", uuid = "uuid3", cards = 5 } )
            , ( "uuid4", { name = "Alexandre", uuid = "uuid4", cards = 6 } )
            ]
    , localPlayer = { name = "Maxime", uuid = "uuid1", hand = List.take 7 allCards }
    , currentPlayer = "uuid1"
    , drawStack = 10
    , activeCard = List.head allCards
    , activeColor = Just Game.Color.Red
    }
