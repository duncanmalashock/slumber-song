module MacOS.Visible exposing (Visible, rect, view, window)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.Visible.Rect
import MacOS.Window


type Visible msg
    = Rect MacOS.Visible.Rect.Config
    | Window (MacOS.Window.Config msg)


rect : MacOS.Visible.Rect.Config -> Visible msg
rect config =
    Rect config


window : MacOS.Window.Config msg -> Visible msg
window config =
    Window config


view : Rect -> Visible msg -> Html msg
view objRect screenObject =
    case screenObject of
        Rect config ->
            MacOS.Visible.Rect.draw config objRect

        Window config ->
            MacOS.Window.view config objRect
