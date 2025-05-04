module MacOS.Visible exposing (Visible, rect, view)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.Visible.Rect


type Visible
    = Rect MacOS.Visible.Rect.Config


rect : MacOS.Visible.Rect.Config -> Visible
rect config =
    Rect config


view : Rect -> Visible -> Html msg
view objRect screenObject =
    case screenObject of
        Rect config ->
            MacOS.Visible.Rect.draw config objRect
