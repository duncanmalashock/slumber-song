module MacOS.ViewHelpers exposing (imgURL, none, px, roundFloat)

import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)


px : Int -> String
px int =
    String.fromInt int ++ "px"


none : Html msg
none =
    Html.text ""


roundFloat : Decoder Int
roundFloat =
    Decode.float |> Decode.map round


imgURL : String -> String
imgURL filename =
    "url(\"" ++ filename ++ "\")"
