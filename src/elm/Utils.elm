module Utils exposing (..)

import Html
import Json.Decode as D
import Json.Encode as E
import Random exposing (Generator)
import Random.List



-- type


type alias UUID =
    String


type alias Code =
    String


type alias RoomData =
    { code : Code
    , playerUUID : UUID
    , username : String
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



-- PSEUDO


type alias Character =
    String


characters : List Character
characters =
    [ "Elmo"
    , "Big Bird"
    , "Cookie Monster"
    , "Oscar the Grouch"
    , "Grover"
    , "Bert"
    , "Ernie"
    , "Count von Count"
    , "Abby Cadabby"
    , "Telly Monster"
    , "Snuffy"
    , "Baby Bear"
    , "Herry Monster"
    , "Biff"
    , "Sully"
    , "Guy Smiley"
    , "Sherlock Hemlock"
    , "The Amazing Mumford"
    , "The Martians"
    , "Alice Snuffleupagus"
    , "Grundgetta"
    , "Murray Monster"
    , "Gladys the Cow"
    , "Forgetful Jones"
    , "Simon Soundman"
    , "Barkley"
    , "Slimey the Worm"
    , "Hoots the Owl"
    , "Frazzle"
    , "Lulu"
    , "Dinger"
    , "Honker"
    , "The Yip Yips"
    , "Feathered Friends"
    , "Super Grover"
    , "Gabi"
    , "Mr. Snuffleupagus"
    , "Emilio"
    , "Mr. Johnson"
    , "Benny Rabbit"
    , "Baby Natasha"
    , "Stinky the Stinkweed"
    , "Sam the Robot"
    , "Little Chrissy"
    , "Mumford"
    , "Stephane Genesis"
    , "Donino"
    , "Biteloutre"
    ]


randomCharacterGenerator : Generator Character
randomCharacterGenerator =
    Random.List.choose characters |> Random.map Tuple.first |> Random.map (Maybe.withDefault "Donino")
