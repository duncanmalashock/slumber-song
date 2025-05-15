module MacOS.UI.View.Textarea exposing
    ( Config
    , view
    , Font(..)
    )

{-| A bitmap image onscreen.


# Config

@docs Config


# View

@docs view

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.UI.Helpers exposing (imgURL, px)


type alias Config =
    { font : Font
    }


type Font
    = Chicago
    | Geneva


view : Config -> Rect -> String -> List (Html msg) -> Html msg
view config objectRect textContent childrenViews =
    div
        ([ style "position" "absolute"
         , style "top" (px (Rect.top objectRect))
         , style "left" (px (Rect.left objectRect))
         , style "width" (px (Rect.width objectRect))
         , style "height" (px (Rect.height objectRect))
         ]
            ++ textStyles config
        )
        [ text textContent
        ]


textStyles : Config -> List (Html.Attribute msg)
textStyles config =
    case config.font of
        Chicago ->
            [ style "background-color" "white"
            , style "color" "black"
            , style "font-family" "Chicago"
            , style "padding" "0 6px"
            ]

        Geneva ->
            [ style "background-color" "white"
            , style "color" "black"
            , style "font-family" "Geneva"
            , style "padding" "0 6px"
            ]
