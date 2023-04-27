module Utils.Utils exposing (..)

import Html



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
