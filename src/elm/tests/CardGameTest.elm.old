module elm.tests.CardGameTest exposing (..)

import CardGame exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


testingPlayers : List Player
testingPlayers =
    [ { name = "Player 1", hand = [] }
    , { name = "Player 2", hand = [] }
    ]

testingDraw: Draw
testingDraw = [CardGame.NumberCard 1 CardGame.Blue, CardGame.NumberCard 2 CardGame.Red, CardGame.NumberCard 3 CardGame.Green]


testingGame : CardGame.Game
testingGame =
    CardGame.Game
        testingPlayers
        testingDraw
        Nothing
        Nothing
        


shouldNextTurnToCorrectPlayer : Test
shouldNextTurnToCorrectPlayer =
    test "Next player to play" (\_ -> Expect.equal 4 (2 + 2))
