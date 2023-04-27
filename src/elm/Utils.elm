module Utils exposing (..)

import Json.Decode as D
import Json.Encode as E
import Html



-- type


type alias UUID =
    String


type alias Code =
    String


type alias RoomData =
    { code : Code
    }



-- Map update with submsg to update with msg


updateWith : (a -> b) -> (c -> msg) -> d -> ( a, Cmd c ) -> ( b, Cmd msg )
updateWith toModel toMsg _ ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- Map view with submsg to view with msg


viewWith : (a -> msg) -> { b | title : c, content : Html.Html a } -> { title : c, body : List (Html.Html msg) }
viewWith toMsg { title, content } =
    { title = title
    , body = [ content |> Html.map toMsg ]
    }



-- Encode/decode maybe
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