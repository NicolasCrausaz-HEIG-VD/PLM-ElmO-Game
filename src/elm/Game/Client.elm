module Game.Client exposing (..)

import Game.Card exposing (Card, PlayableCard(..))
import Game.Color exposing (Color)
import Game.Core exposing (Hand)
import Json.Decode as D
import Utils exposing (UUID)


type alias DistantPlayer =
    { name : String
    , uuid : UUID
    , cards : Int
    }


type alias LocalPlayer =
    { name : String
    , uuid : UUID
    , hand : Hand
    , saidUno : Bool
    }


type alias Model =
    { distantPlayers : List DistantPlayer
    , localPlayer : LocalPlayer
    , currentPlayer : UUID
    , drawStack : Int
    , activeCard : Maybe Card
    , activeColor : Maybe Color
    , gameOver : Bool
    }


showUnoButton : Model -> Bool
showUnoButton model =
    List.length model.localPlayer.hand == 1 && not model.localPlayer.saidUno && model.currentPlayer /= model.localPlayer.uuid


hintDrawCard : Model -> Bool
hintDrawCard model =
    (model.currentPlayer == model.localPlayer.uuid) && (model.localPlayer.hand |> Game.Card.getPlayableCards ( model.activeCard, model.activeColor ) |> List.isEmpty)


hintPlayCard : Model -> Card -> Bool
hintPlayCard model card =
    (model.currentPlayer == model.localPlayer.uuid) && Game.Card.canPlayCard ( model.activeCard, model.activeColor ) card


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


decodeLocalPlayer : D.Decoder LocalPlayer
decodeLocalPlayer =
    D.map4 LocalPlayer
        (D.field "name" D.string)
        (D.field "uuid" D.string)
        (D.field "hand" (D.list Game.Card.decodeCard))
        (D.field "saidUno" D.bool)


decodeModel : D.Decoder Model
decodeModel =
    D.map7 Model
        (D.field "distantPlayers" (D.list decodeDistantPlayer))
        (D.field "localPlayer" decodeLocalPlayer)
        (D.field "currentPlayer" D.string)
        (D.field "drawStack" D.int)
        (D.field "activeCard" (D.nullable Game.Card.decodeCard))
        (D.field "activeColor" (D.nullable Game.Color.decodeColor))
        (D.field "gameOver" D.bool)
