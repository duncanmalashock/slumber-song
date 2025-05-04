module MacOS.UIObject exposing (UIObject, containsCoordinate, new, position, rect, setPosition, view, visible)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.ViewHelpers as ViewHelpers
import MacOS.Visible as Visible exposing (Visible)


type UIObject
    = UIObject Internals


type alias Internals =
    { rect : Rect
    , visible : Maybe Visible
    , draggable : Maybe Draggable
    }


type Draggable
    = Draggable { traveling : Visible }


new : { rect : Rect } -> UIObject
new params =
    UIObject
        { rect = params.rect
        , visible = Nothing
        , draggable = Nothing
        }


visible : Visible -> UIObject -> UIObject
visible vis (UIObject internals) =
    UIObject
        { internals
            | visible = Just vis
        }


rect : UIObject -> Rect
rect (UIObject internals) =
    internals.rect


position : UIObject -> Coordinate
position (UIObject internals) =
    Rect.position internals.rect


setPosition : Coordinate -> UIObject -> UIObject
setPosition newValue (UIObject internals) =
    UIObject
        { internals
            | rect = Rect.setPosition newValue internals.rect
        }


view : UIObject -> Html msg
view (UIObject internals) =
    case internals.visible of
        Just vis ->
            Visible.view internals.rect vis

        Nothing ->
            ViewHelpers.none


containsCoordinate : Coordinate -> UIObject -> Bool
containsCoordinate coordinate (UIObject internals) =
    Rect.containsCoordinate coordinate internals.rect
