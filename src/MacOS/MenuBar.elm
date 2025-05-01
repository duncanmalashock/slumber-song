module MacOS.MenuBar exposing (MenuBar, menu, new, view)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events
import MacOS.FillPattern as FillPattern
import MacOS.Rect as Rect exposing (Rect)
import MacOS.ViewHelpers as ViewHelpers exposing (px)


type MenuBar
    = MenuBar Internals


type alias Internals =
    { mode : Mode
    , menus : List Menu
    }


type Mode
    = Hidden
    | Blank
    | AppTitle
    | Ready


new : List Menu -> MenuBar
new menus =
    MenuBar
        { mode = Ready
        , menus = menus
        }


type Menu
    = Menu String Bool


menu : String -> Bool -> Menu
menu title disabled =
    Menu title disabled


view : Rect -> MenuBar -> Html msg
view screen (MenuBar { menus }) =
    let
        viewMenus : List (Html msg)
        viewMenus =
            menuButton appleLogo False
                :: List.map
                    (\(Menu title isDisabled) ->
                        menuButton title isDisabled
                    )
                    menus
    in
    div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "background" "black"
        ]
        [ div
            [ style "width" (px (Rect.width screen))
            , style "height" "20px"
            , style "background" "white"
            , style "border-bottom" "solid 1px"
            , class "menu-bar"
            , style "padding" "0 10px"
            ]
            viewMenus
        ]


menuButton : String -> Bool -> Html msg
menuButton label isDisabled =
    if isDisabled then
        button
            [ style "padding" "1px 10px 2px"
            , style "background-image" FillPattern.dither50
            , style "text-shadow" "none"
            , style "background-clip" "text"
            , style "color" "transparent"
            , style "background-position" "2px 1.4px"
            , disabled True
            ]
            [ text label ]

    else
        button [ style "padding" "1px 10px 2px" ] [ text label ]


appleLogo : String
appleLogo =
    "¤"
