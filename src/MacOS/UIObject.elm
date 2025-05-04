module MacOS.UIObject exposing (UIObject, containsCoordinate, new, view)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Rect as Rect exposing (Rect)


type UIObject
    = UIObject Internals


type alias Internals =
    { rect : Rect
    }


new : { rect : Rect } -> UIObject
new params =
    UIObject
        { rect = params.rect
        }


view : UIObject -> Html msg
view (UIObject internals) =
    Rect.drawSolidFilled internals.rect


containsCoordinate : Coordinate -> UIObject -> Bool
containsCoordinate coordinate (UIObject internals) =
    Rect.containsCoordinate coordinate internals.rect
