module Game.Client exposing (..)

import Dict exposing (Dict)
import Game.Card exposing (Card, PlayableCard(..))
import Game.Color exposing (Color)
import Game.Core exposing (Hand, Player, allCards)
import Utils exposing (UUID)
import Json.Decode as D
import Json.Encode as E


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



encodeDistantPlayer : DistantPlayer -> E.Value
encodeDistantPlayer player =
    E.object
        [ ( "name", E.string player.name )
        , ( "uuid", E.string player.uuid )
        , ( "cards", E.int player.cards )
        ]


decodeDistantPlayer : D.Decoder DistantPlayer
decodeDistantPlayer =
    D.map3 DistantPlayer
        (D.field "name" D.string)
        (D.field "uuid" D.string)
        (D.field "cards" D.int)


encodeLocalPlayer : LocalPlayer -> E.Value
encodeLocalPlayer player =
    E.object
        [ ( "name", E.string player.name )
        , ( "uuid", E.string player.uuid )
        , ( "hand", E.list Game.Card.encodeCard player.hand )
        ]


decodeLocalPlayer : D.Decoder LocalPlayer
decodeLocalPlayer =
    D.map3 LocalPlayer
        (D.field "name" D.string)
        (D.field "uuid" D.string)
        (D.field "hand" (D.list Game.Card.decodeCard))


encodeModel : Model -> E.Value
encodeModel model =
    E.object
        [ ( "distantPlayers", E.list encodeDistantPlayer (Dict.values model.distantPlayers) )
        , ( "localPlayer", encodeLocalPlayer model.localPlayer )
        , ( "currentPlayer", E.string model.currentPlayer )
        , ( "drawStack", E.int model.drawStack )
        , ( "activeCard", Utils.maybeEncode Game.Card.encodeCard model.activeCard )
        , ( "activeColor", Utils.maybeEncode Game.Color.encodeColor model.activeColor )
        ]


decodeModel : D.Decoder Model
decodeModel =
    D.map6 Model
        (D.field "distantPlayers" (D.list decodeDistantPlayer)
            |> D.andThen
                (\players ->
                    Dict.fromList (List.map (\p -> ( p.uuid, p )) players)
                        |> D.succeed
                )
        )
        (D.field "localPlayer" decodeLocalPlayer)
        (D.field "currentPlayer" D.string)
        (D.field "drawStack" D.int)
        (D.field "activeCard" (D.nullable Game.Card.decodeCard))
        (D.field "activeColor" (D.nullable Game.Color.decodeColor))
