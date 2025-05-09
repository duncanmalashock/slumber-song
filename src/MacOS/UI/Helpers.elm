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
    { os = "MacOS"
    , root = "MacOS__UI_ROOT"
    , debugger = "MacOS__DEBUG"
    , desktop = "MacOS__DESKTOP"
    , desktopRectangles = "MacOS__DESKTOP_RECTANGLES"
    , menuBar = "MacOS__MENUBAR"
    , screenCorners = "MacOS__SCREEN_CORNERS"
    , mouse = "MacOS__MOUSE"
    }
