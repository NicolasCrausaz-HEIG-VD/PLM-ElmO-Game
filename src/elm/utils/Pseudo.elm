module Utils.Pseudo exposing (..)

import Random exposing (Generator)
import Random.List


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
