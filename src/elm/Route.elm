module Route exposing (..)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf)


type Route
    = Lobby
    | Room String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Lobby Parser.top
        , Parser.map Room (Parser.s "room" </> Parser.string)
        ]


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


routeToString : Route -> String
routeToString route =
    "/" ++ String.join "/" (routeToPieces route)


routeToPieces : Route -> List String
routeToPieces route =
    case route of
        Lobby ->
            [ "lobby" ]

        Room code ->
            [ "room", code ]
