module MacOS.UI.View.Image exposing (view)

{-| A bitmap image onscreen.


# View

@docs view

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.UI.Helpers exposing (imgURL, px)


view : { url : String, size : ( Int, Int ) } -> Rect -> List (Html msg) -> Html msg
view params rect childrenViews =
    div
        [ style "position" "absolute"
        , style "top" (px (Rect.top rect))
        , style "left" (px (Rect.left rect))
        , style "width" (px (Rect.width rect))
        , style "height" (px (Rect.height rect))
        , style "background-image" (imgURL params.url)
        ]
        childrenViews
