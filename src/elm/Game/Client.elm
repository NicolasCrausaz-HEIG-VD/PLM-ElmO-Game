module Game.Client exposing (..)

import Game.Action exposing (Action)
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
    , turn : Int
    , action : Action
    }


isPlayerTurn : Model -> Bool
isPlayerTurn model =
    model.currentPlayer == model.localPlayer.uuid


showUnoButton : Model -> Bool
showUnoButton model =
    List.length model.localPlayer.hand == 1 && not model.localPlayer.saidUno && not (model |> isPlayerTurn)


hintDrawCard : Model -> Bool
hintDrawCard model =
    (model |> isPlayerTurn) && (model.localPlayer.hand |> Game.Card.getPlayableCards ( model.activeCard, model.activeColor ) |> List.isEmpty)


hintPlayCard : Model -> Card -> Bool
hintPlayCard model card =
    (model |> isPlayerTurn) && Game.Card.canPlayCard ( model.activeCard, model.activeColor ) card


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


decodeApply : D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
decodeApply =
    D.map2 (|>)


required : String -> D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
required fieldName itemDecoder functionDecoder =
    decodeApply (D.field fieldName itemDecoder) functionDecoder


decodeModel : D.Decoder Model
decodeModel =
    D.succeed Model
        |> required "distantPlayers" (D.list decodeDistantPlayer)
        |> required "localPlayer" decodeLocalPlayer
        |> required "currentPlayer" D.string
        |> required "drawStack" D.int
        |> required "activeCard" (D.nullable Game.Card.decodeCard)
        |> required "activeColor" (D.nullable Game.Color.decodeColor)
        |> required "gameOver" D.bool
        |> required "turn" D.int
        |> required "action" Game.Action.decodeAction
