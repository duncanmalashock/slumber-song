module MacOS.UI.View.Textarea exposing
    ( Config
    , view
    , Color(..), Font(..)
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
    , color : Color
    }


type Font
    = Chicago
    | Geneva


type Color
    = Black
    | White


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
    let
        colorStyles : List (Html.Attribute msg)
        colorStyles =
            case config.color of
                Black ->
                    [ style "color" "black" ]

                White ->
                    [ style "color" "white" ]

        fontStyles : List (Html.Attribute msg)
        fontStyles =
            case config.font of
                Chicago ->
                    [ style "font-family" "Chicago"
                    , style "line-height" "16px"
                    , style "padding" "0 4px"
                    , style "word-spacing" "-2px"
                    , style "white-space" "break-spaces"
                    , style "transform" "translate(0,-0.5px)"
                    ]

                Geneva ->
                    [ style "font-family" "Geneva"
                    , style "line-height" "12px"
                    , style "padding" "0 4px"
                    ]
    in
    fontStyles ++ colorStyles
