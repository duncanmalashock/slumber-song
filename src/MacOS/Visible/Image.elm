module MacOS.Visible.Image exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.ViewHelpers exposing (imgURL, px)


view : { url : String, size : ( Int, Int ) } -> Rect -> Html msg
view params rect =
    let
        _ =
            Debug.log "Image.view" ""
    in
    div
        [ style "position" "absolute"
        , style "top" (px (Rect.top rect))
        , style "left" (px (Rect.left rect))
        , style "width" (px (Rect.width rect))
        , style "height" (px (Rect.height rect))
        , style "background-image" (imgURL params.url)
        ]
        []
