module CardView exposing (..)

import Card exposing (Card)
import Html exposing (Attribute, Html, div, img)
import Html.Attributes exposing (class, classList, src, style)

type alias CardProps =
    { size : String
    , flipped : Bool
    }

-- Passing Attributes to a view to allow for to add attributes like event handlers
view : List (Attribute msg) -> CardProps -> Card -> Html msg
view attributes { size, flipped } card =
        div
            (attributes ++ [class "card", classList [ ( "active", flipped ) ]])
            [ div
                [ class "card_inner" ]
                [ img
                    [ src ("/cards/" ++ Card.toString card ++ ".svg")
                    , style "height" size
                    , class "card_front"
                    ]
                    []
                , img
                    [ src "/cards/empty.svg"
                    , class "card_back"
                    ]
                    []
                ]
            ]
