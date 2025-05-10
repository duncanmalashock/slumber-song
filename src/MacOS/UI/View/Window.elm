module MacOS.UI.View.Window exposing (Window, view)

{-| A window containing other `View`s, configurable with different visual settings.


# View

@docs Window, view

-}

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Mouse as Mouse exposing (Mouse)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.UI.Helpers as UIHelpers exposing (px)


type alias Window msg =
    { title : String
    , closeMsg : Maybe msg
    , rect : Rect
    }


view : Window msg -> Bool -> Rect -> List (Html msg) -> Html msg
view config isActive rect childrenViews =
    div
        [ style "position" "absolute"
        , style "top" (px (Rect.posY rect))
        , style "left" (px (Rect.posX rect))
        , style "width" (px (Rect.width rect))
        , style "height" (px (Rect.height rect))
        , style "background" "white"
        , style "border" "solid 1px"
        , style "box-shadow" "1px 1px 0px"
        , style "overflow" "hidden"
        ]
        [ div [] childrenViews
        , viewWindowTitle config isActive
        ]


viewWindowTitle : Window msg -> Bool -> Html msg
viewWindowTitle config isActive =
    div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "right" "0"
        , style "background" "white"
        , style "border-bottom" "solid 1px"
        , style "text-align" "center"
        , style "height" "18px"
        ]
        [ if isActive then
            viewWindowTitleLines

          else
            UIHelpers.none
        , if isActive then
            case config.closeMsg of
                Just msg ->
                    viewWindowCloseBox msg

                Nothing ->
                    UIHelpers.none

          else
            UIHelpers.none
        , span
            [ style "position" "relative"
            , style "background" "white"
            , style "padding" "0 5px"
            , style "display" "inline-block"
            , style "transform" "translateY(0.5px)"
            , style "pointer-events" "none"
            ]
            [ text config.title ]
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
