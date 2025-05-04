module MacOS.Rect exposing (Rect, addPosition, bottom, containsCoordinate, drawDotted, drawSolid, drawSolidFilled, height, left, new, posX, posY, position, right, size, top, width)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.FillPattern as FillPattern
import MacOS.ViewHelpers as ViewHelpers exposing (imgURL, px)


type Rect
    = Rect { position : Coordinate, size : Coordinate }


new : ( Int, Int ) -> ( Int, Int ) -> Rect
new ( x, y ) ( w, h ) =
    Rect
        { position = Coordinate.new ( x, y )
        , size = Coordinate.new ( w, h )
        }


position : Rect -> Coordinate
position (Rect internals) =
    internals.position


containsCoordinate : Coordinate -> Rect -> Bool
containsCoordinate coordinate rect =
    let
        px : Int
        px =
            Coordinate.x coordinate

        py : Int
        py =
            Coordinate.y coordinate
    in
    (px >= left rect)
        && (px <= right rect)
        && (py >= top rect)
        && (py <= bottom rect)


posX : Rect -> Int
posX (Rect internals) =
    Coordinate.x internals.position


posY : Rect -> Int
posY (Rect internals) =
    Coordinate.y internals.position


addPosition : Coordinate -> Rect -> Rect
addPosition coordinate (Rect internals) =
    Rect
        { internals
            | position = Coordinate.plus coordinate internals.position
        }


width : Rect -> Int
width (Rect internals) =
    Coordinate.x internals.size


height : Rect -> Int
height (Rect internals) =
    Coordinate.y internals.size


size : Rect -> Coordinate
size (Rect internals) =
    internals.size


top : Rect -> Int
top (Rect internals) =
    Coordinate.y internals.position


bottom : Rect -> Int
bottom (Rect internals) =
    Coordinate.y internals.position
        + Coordinate.y internals.size


left : Rect -> Int
left (Rect internals) =
    Coordinate.x internals.position


right : Rect -> Int
right (Rect internals) =
    Coordinate.x internals.position
        + Coordinate.x internals.size


type LineStyle
    = LineStyleSolid
    | LineStyleDotted
    | LineStyleSolidFilled


drawSolid : Rect -> Html msg
drawSolid rect =
    draw rect LineStyleSolid


drawSolidFilled : Rect -> Html msg
drawSolidFilled rect =
    draw rect LineStyleSolidFilled


drawDotted : Rect -> Html msg
drawDotted rect =
    draw rect LineStyleDotted


draw : Rect -> LineStyle -> Html msg
draw rect lineStyle =
    let
        lineStyleAttrs : List (Html.Attribute msg)
        lineStyleAttrs =
            case lineStyle of
                LineStyleSolid ->
                    [ style "background" "black"
                    , style "mix-blend-mode" "multiply"
                    ]

                LineStyleSolidFilled ->
                    [ style "background" "black"
                    ]

                LineStyleDotted ->
                    let
                        alternatingDotPosition : List (Html.Attribute msg)
                        alternatingDotPosition =
                            -- Very cool trick:
                            -- using an alternating dot pattern make the line always
                            -- dotted over white and always black over 50% dither
                            if modBy 2 (top rect + left rect) == 0 then
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
         , style "top" (px (top rect - 1))
         , style "left" (px (left rect))
         , style "width" (px (width rect))
         , style "height" (px (height rect))
         , style "pointer-events" "none"
         ]
            ++ lineStyleAttrs
        )
        [ div
            [ style "position" "relative"
            , style "top" (px 1)
            , style "left" (px 1)
            , style "width" (px (width rect - 2))
            , style "height" (px (height rect - 2))
            , style "background" "white"
            ]
            []
        ]
