module MacOS.UI.Helpers exposing (domIds, imgURL, none, px)

import Html exposing (Html)


px : Int -> String
px int =
    String.fromInt int ++ "px"


none : Html msg
none =
    Html.text ""


imgURL : String -> String
imgURL filename =
    "url(\"" ++ filename ++ "\")"


domIds =
    let
        addDomIdPrefix : String -> String
        addDomIdPrefix id =
            "MacOS__" ++ id
    in
    { root = addDomIdPrefix "UI_ROOT"
    , debugger = addDomIdPrefix "DEBUG"
    , desktop = addDomIdPrefix "DESKTOP"
    , desktopRectangles = addDomIdPrefix "DESKTOP_RECTANGLES"
    , menuBar = addDomIdPrefix "MENU_BAR"
    , screenCorners = addDomIdPrefix "SCREEN_CORNERS"
    , mouse = addDomIdPrefix "MOUSE"
    }
