module MacOS.UI.View.Rect exposing
    ( Config(..)
    , view
    )

{-| A rectangular shape onscreen.


# Config

@docs Config


# View

@docs view

-}

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.UI.FillPattern as FillPattern
import MacOS.UI.Helpers exposing (imgURL, px)


type Config
    = StyleSolid
    | StyleDotted
    | StyleSolidFilled
    | StyleFillBlack


view : Config -> Rect -> List (Html msg) -> Html msg
view config rect childrenViews =
    let
        lineStyleAttrs : List (Html.Attribute msg)
        lineStyleAttrs =
            case config of
                StyleSolid ->
                    [ style "background" "black"
                    ]

                StyleFillBlack ->
                    [ style "background" "white"
                    ]

                StyleSolidFilled ->
                    [ style "background" "black"
                    ]

                StyleDotted ->
                    let
                        alternatingDotPosition : List (Html.Attribute msg)
                        alternatingDotPosition =
                            -- Very cool trick:
                            -- using an alternating dot pattern make the line always
                            -- dotted over white and always black over 50% dither
                            if modBy 2 (Rect.top rect + Rect.left rect) == 0 then
                                []

                            else
                                [ style "background-position-x" "1px" ]
                    in
                    [ style "background-image" FillPattern.dither50
                    , style "mix-blend-mode" "multiply"
                    ]
                        ++ alternatingDotPosition
    in
    div
        ([ style "position" "absolute"
         , style "top" (px (Rect.top rect - 1))
         , style "left" (px (Rect.left rect))
         , style "width" (px (Rect.width rect))
         , style "height" (px (Rect.height rect))
         , style "pointer-events" "none"
         ]
            ++ lineStyleAttrs
        )
        [ div
            [ style "position" "relative"
            , style "top" (px 1)
            , style "left" (px 1)
            , style "width" (px (Rect.width rect - 2))
            , style "height" (px (Rect.height rect - 2))
            , case config of
                StyleSolid ->
                    style "background" "white"

                StyleFillBlack ->
                    style "background" "black"

                StyleSolidFilled ->
                    style "background" "white"

                StyleDotted ->
                    style "background" "white"
            ]
            childrenViews
        ]
