module MacOS.UI.Helpers exposing (imgURL, none, px)

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
