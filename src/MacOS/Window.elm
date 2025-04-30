module MacOS.Window exposing (DragInfo, Window, view)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.FillPattern as FillPattern
import MacOS.Rect as Rect exposing (Rect)
import MacOS.ViewHelpers as ViewHelpers exposing (px)


type alias Window =
    { title : String
    , rect : Rect
    }


type alias DragInfo =
    { window : Window
    , offset : Coordinate
    , cursorAtDragStart : Coordinate
    , cursor : Coordinate
    }


view : (DragInfo -> msg) -> (Window -> msg) -> Bool -> Window -> Html msg
view mouseDownToMsg onClickMsg isActive ({ title, rect } as window) =
    div
        [ style "position" "absolute"
        , style "z-index" "1"
        , style "top" (px (Rect.posY rect))
        , style "left" (px (Rect.posX rect))
        , style "width" (px (Rect.width rect))
        , style "height" (px (Rect.height rect))
        , style "background" "white"
        , style "background-image" FillPattern.dither25
        , style "border" "solid 1px"
        , style "box-shadow" "1px 1px 0px"
        , Events.stopPropagationOn "pointerdown" (Decode.succeed ( onClickMsg window, True ))
        ]
        [ viewWindowTitle mouseDownToMsg isActive window

        --, div [] content
        ]


viewWindowTitle : (DragInfo -> msg) -> Bool -> Window -> Html msg
viewWindowTitle mouseDownToMsg isActive window =
    div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "right" "0"
        , style "background" "white"
        , style "border-bottom" "solid 1px"
        , style "text-align" "center"
        , style "height" "18px"
        , onPointerDownForWindowTitle mouseDownToMsg window
        ]
        [ if isActive then
            viewWindowTitleLines

          else
            ViewHelpers.none
        , span
            [ style "position" "relative"
            , style "z-index" "2"
            , style "background" "white"
            , style "padding" "0 5px"
            , style "display" "inline-block"
            , style "transform" "translateY(0.5px)"
            , style "pointer-events" "none"
            ]
            [ text window.title ]
        ]


onPointerDownForWindowTitle : (DragInfo -> msg) -> Window -> Attribute msg
onPointerDownForWindowTitle mouseDownToMsg window =
    Events.stopPropagationOn "pointerdown"
        (Decode.map4
            (\cx cy ox oy ->
                ( mouseDownToMsg
                    { offset = Coordinate.new ( ox + 1, oy )
                    , cursorAtDragStart = Coordinate.new ( cx, cy )
                    , cursor = Coordinate.new ( cx, cy )
                    , window = window
                    }
                , True
                )
            )
            (Decode.field "clientX" ViewHelpers.roundFloat)
            (Decode.field "clientY" ViewHelpers.roundFloat)
            (Decode.field "offsetX" ViewHelpers.roundFloat)
            (Decode.field "offsetY" ViewHelpers.roundFloat)
        )


viewWindowTitleLines : Html msg
viewWindowTitleLines =
    let
        line : Html msg
        line =
            div
                [ style "width" "100%"
                , style "height" "1px"
                , style "background" "black"
                ]
                []
    in
    div
        [ style "position" "absolute"
        , style "z-index" "1"
        , style "inset" "0"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "1px"
        , style "padding" "3px 1px"
        , style "pointer-events" "none"
        ]
        (List.repeat 6 line)
