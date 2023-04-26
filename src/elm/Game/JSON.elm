module Game.JSON exposing (..)

import Dict exposing (Dict)
import Game.Card
import Game.Client
import Game.Color
import Game.Host
import Json.Decode as D
import Json.Encode as E



-- Utils


maybeEncode : (a -> E.Value) -> Maybe a -> E.Value
maybeEncode encoder maybeValue =
    case maybeValue of
        Just value ->
            encoder value

        Nothing ->
            E.null


decodeMaybe : D.Decoder a -> (a -> Maybe b) -> D.Decoder b
decodeMaybe decoder toValue =
    decoder
        |> D.andThen
            (\value ->
                case toValue value of
                    Just v ->
                        D.succeed v

                    Nothing ->
                        D.fail "Error decoding value: "
            )



-- Action


encodeAction : Game.Host.Action -> E.Value
encodeAction action =
    case action of
        Game.Host.PlayCard uuid card ->
            E.object
                [ ( "action", E.string "playCard" )
                , ( "uuid", E.string uuid )
                , ( "card", encodePlayableCard card )
                ]

        Game.Host.DrawCard uuid ->
            E.object
                [ ( "action", E.string "drawCard" )
                , ( "uuid", E.string uuid )
                ]

        _ ->
            E.object []


decodeAction : D.Decoder Game.Host.Action
decodeAction =
    D.field "action" D.string
        |> D.andThen
            (\action ->
                case action of
                    "playCard" ->
                        D.map2 Game.Host.PlayCard
                            (D.field "uuid" D.string)
                            (D.field "card" decodePlayableCard)

                    "drawCard" ->
                        D.map Game.Host.DrawCard
                            (D.field "uuid" D.string)

                    _ ->
                        D.fail "Error decoding action: "
            )



-- Card


encodeCard : Game.Card.Card -> E.Value
encodeCard card =
    E.string (Game.Card.toString card)


decodeCard : D.Decoder Game.Card.Card
decodeCard =
    decodeMaybe D.string Game.Card.fromString



-- Color


encodeColor : Game.Color.Color -> E.Value
encodeColor color =
    E.string (Game.Color.toString color)


decodeColor : D.Decoder Game.Color.Color
decodeColor =
    decodeMaybe D.string Game.Color.fromString



-- PlayableCard


encodePlayableCard : Game.Card.PlayableCard -> E.Value
encodePlayableCard card =
    case card of
        Game.Card.StandardCard c ->
            E.object
                [ ( "card", E.string (Game.Card.toString c) )
                , ( "color", E.null )
                ]

        Game.Card.ChoiceCard c color ->
            E.object
                [ ( "card", E.string (Game.Card.toString c) )
                , ( "color", E.string (Game.Color.toString color) )
                ]


decodePlayableCard : D.Decoder Game.Card.PlayableCard
decodePlayableCard =
    D.map2
        (\card color ->
            case color of
                Just c ->
                    Game.Card.ChoiceCard card c

                Nothing ->
                    Game.Card.StandardCard card
        )
        (D.field "card" decodeCard)
        (D.field "color" (D.nullable decodeColor))



-- DistandPlayer


encodeDistantPlayer : Game.Client.DistantPlayer -> E.Value
encodeDistantPlayer player =
    E.object
        [ ( "name", E.string player.name )
        , ( "uuid", E.string player.uuid )
        , ( "cards", E.int player.cards )
        ]


decodeDistantPlayer : D.Decoder Game.Client.DistantPlayer
decodeDistantPlayer =
    D.map3 Game.Client.DistantPlayer
        (D.field "name" D.string)
        (D.field "uuid" D.string)
        (D.field "cards" D.int)



-- LocalPlayer


encodeLocalPlayer : Game.Client.LocalPlayer -> E.Value
encodeLocalPlayer player =
    E.object
        [ ( "name", E.string player.name )
        , ( "uuid", E.string player.uuid )
        , ( "hand", E.list encodeCard player.hand )
        ]


decodeLocalPlayer : D.Decoder Game.Client.LocalPlayer
decodeLocalPlayer =
    D.map3 Game.Client.LocalPlayer
        (D.field "name" D.string)
        (D.field "uuid" D.string)
        (D.field "hand" (D.list decodeCard))



-- Client Model


encodeModel : Game.Client.Model -> E.Value
encodeModel model =
    E.object
        [ ( "distantPlayers", E.list encodeDistantPlayer (Dict.values model.distantPlayers) )
        , ( "localPlayer", encodeLocalPlayer model.localPlayer )
        , ( "currentPlayer", E.string model.currentPlayer )
        , ( "drawStack", E.int model.drawStack )
        , ( "activeCard", maybeEncode encodeCard model.activeCard )
        , ( "activeColor", maybeEncode encodeColor model.activeColor )
        ]


decodeModel : D.Decoder Game.Client.Model
decodeModel =
    D.map6 Game.Client.Model
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
        (D.field "activeCard" (D.nullable decodeCard))
        (D.field "activeColor" (D.nullable decodeColor))
