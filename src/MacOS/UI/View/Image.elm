module MacOS.UI.View.Image exposing
    ( Config
    , Filter(..)
    , view
    )

{-| A bitmap image onscreen.


# Config

@docs Config
@docs Filter


# View

@docs view

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.UI.Helpers exposing (imgURL, px)


type alias Config =
    { url : String
    , size : ( Int, Int )
    , filter : Maybe Filter
    }


type Filter
    = Invert


view : Config -> Rect -> List (Html msg) -> Html msg
view config objectRect childrenViews =
    let
        filterStyles : List (Html.Attribute msg)
        filterStyles =
            case config.filter of
                Just Invert ->
                    [ style "filter" "invert(1)"
                    ]

                Nothing ->
                    []
    in
    div
        ([ style "position" "absolute"
         , style "top" (px (Rect.top objectRect))
         , style "left" (px (Rect.left objectRect))
         , style "width" (px (Rect.width objectRect))
         , style "height" (px (Rect.height objectRect))
         , style "background-image" (imgURL config.url)
         ]
            ++ filterStyles
        )
        childrenViews
