module MacOS.MenuBar exposing (view)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events
import MacOS.FillPattern as FillPattern


view : Html msg
view =
    div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "background" "black"
        ]
        [ div
            [ style "width" "512px"
            , style "height" "19px"
            , style "background" "white"
            , style "border-top-left-radius" "4px"
            , style "border-top-right-radius" "4px"
            , style "border-bottom" "solid 1px"
            , class "menu-bar"
            , style "padding" "0 10px"
            ]
            [ menuButton appleLogo False
            , menuButton "File" False
            , menuButton "Edit" True
            , menuButton "Special" True
            , menuButton "Font" False
            , menuButton "FontSize" False
            ]
        ]


menuButton : String -> Bool -> Html msg
menuButton label isDisabled =
    if isDisabled then
        button
            [ style "padding" "1px 10px"
            , style "background-image" FillPattern.dither50
            , style "text-shadow" "none"
            , style "background-clip" "text"
            , style "color" "transparent"
            , style "background-position" "2px 1.4px"
            , disabled True
            ]
            [ text label ]

    else
        button [ style "padding" "1px 10px" ] [ text label ]


appleLogo : String
appleLogo =
    "¤"
