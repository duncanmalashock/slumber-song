module MacOS.UI.Helpers exposing (debugColorize, domIds, imgURL, none, px)

import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)


px : Int -> String
px int =
    String.fromInt int ++ "px"


none : Html msg
none =
    Html.text ""


imgURL : String -> String
imgURL filename =
    "url(\"" ++ filename ++ "\")"


debugColorize : Attribute msg
debugColorize =
    style "filter" "contrast(0.5) sepia(1) saturate(25) hue-rotate(-29deg)"


domIds =
    let
        addDomIdPrefix : String -> String
        addDomIdPrefix id =
            "MacOS__" ++ id
    in
    { root = addDomIdPrefix "UI_ROOT"
    , debugger = addDomIdPrefix "DEBUG"
    , desktop = addDomIdPrefix "DESKTOP"
    , windows = addDomIdPrefix "WINDOWS"
    , menuBar = addDomIdPrefix "MENU_BAR"
    , screenCorners = addDomIdPrefix "SCREEN_CORNERS"
    , mouse = addDomIdPrefix "MOUSE"
    }
