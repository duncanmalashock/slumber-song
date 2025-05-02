module MacOS.Window exposing (DragInfo, Window, view)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import MacOS.Context as Context exposing (Context)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.FillPattern as FillPattern
import MacOS.Mouse as Mouse exposing (Mouse)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.ViewHelpers as ViewHelpers exposing (..)


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


view : Context msg -> msg -> (String -> msg) -> msg -> Bool -> Window -> Html msg
view context closeBoxMsg mouseDownToMsg onClickMsg isActive ({ title, rect } as window) =
    div
        ([ style "position" "absolute"
         , style "z-index" "1"
         , style "top" (px (Rect.posY rect))
         , style "left" (px (Rect.posX rect))
         , style "width" (px (Rect.width rect))
         , style "height" (px (Rect.height rect))
         , style "background" "white"
         , style "border" "solid 1px"
         , style "box-shadow" "1px 1px 0px"
         ]
            ++ context.listenersForObject
                { id = "window:disk"
                , coordinate = Rect.position rect
                }
        )
        [ viewWindowTitle context (Rect.position rect) closeBoxMsg mouseDownToMsg isActive window

        --, div [] content
        ]


viewWindowTitle : Context msg -> Coordinate -> msg -> (String -> msg) -> Bool -> Window -> Html msg
viewWindowTitle context position closeBoxMsg mouseDownToMsg isActive window =
    div
        ([ style "position" "absolute"
         , style "top" "0"
         , style "left" "0"
         , style "right" "0"
         , style "background" "white"
         , style "border-bottom" "solid 1px"
         , style "text-align" "center"
         , style "height" "18px"
         ]
            ++ context.listenersForObject
                { id = "window:title:disk"
                , coordinate = position
                }
        )
        [ if isActive then
            viewWindowTitleLines

          else
            ViewHelpers.none
        , if isActive then
            viewWindowCloseBox closeBoxMsg

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


viewWindowCloseBox : msg -> Html msg
viewWindowCloseBox closeBoxMsg =
    div
        [ style "position" "absolute"
        , style "z-index" "1"
        , style "top" (px 3)
        , style "left" (px 7)
        , style "width" (px 13)
        , style "height" (px 11)
        , style "background" "white"
        ]
        [ div
            [ style "position" "absolute"
            , style "width" (px 11)
            , style "height" (px 11)
            , style "left" (px 1)
            , style "border" "1px solid black"
            , style "background" "white"
            , onClick closeBoxMsg

            -- , style "background-image" (imgURL "MacOS/closebox.gif")
            ]
            []
        ]
