module MacOS.Visible.Rect exposing (Config(..), draw)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import MacOS.FillPattern as FillPattern
import MacOS.Rect as Rect exposing (Rect)
import MacOS.ViewHelpers as ViewHelpers exposing (imgURL, px)


type Config
    = StyleSolid
    | StyleDotted
    | StyleSolidFilled


draw : Config -> Rect -> Html msg
draw config rect =
    let
        lineStyleAttrs : List (Html.Attribute msg)
        lineStyleAttrs =
            case config of
                StyleSolid ->
                    [ style "background" "black"
                    , style "mix-blend-mode" "multiply"
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
         , style "z-index" "1"
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
            , style "background" "white"
            ]
            []
        ]
