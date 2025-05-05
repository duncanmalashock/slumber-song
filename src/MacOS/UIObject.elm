module MacOS.UIObject exposing (UIObject, containsCoordinate, draggable, getDraggable, new, position, rect, selectable, setPosition, setSelected, view, visible)

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
    , selectable : Maybe Selectable
    , draggable : Maybe Draggable
    }


type alias Draggable =
    { traveling : Visible }


type alias Selectable =
    { view : Visible
    , selected : Bool
    }


new : { rect : Rect } -> UIObject
new params =
    UIObject
        { rect = params.rect
        , visible = Nothing
        , selectable = Nothing
        , draggable = Nothing
        }


visible : Visible -> UIObject -> UIObject
visible vis (UIObject internals) =
    UIObject
        { internals
            | visible = Just vis
        }


selectable : Selectable -> UIObject -> UIObject
selectable vis (UIObject internals) =
    UIObject
        { internals
            | selectable = Just vis
        }


draggable : Draggable -> UIObject -> UIObject
draggable params (UIObject internals) =
    UIObject
        { internals
            | draggable =
                Just
                    { traveling = params.traveling
                    }
        }


getDraggable : UIObject -> Maybe Draggable
getDraggable (UIObject internals) =
    internals.draggable


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


setSelected : Bool -> UIObject -> UIObject
setSelected newValue (UIObject internals) =
    UIObject
        { internals
            | selectable =
                case internals.selectable of
                    Just s ->
                        Just
                            { s
                                | selected = newValue
                            }

                    Nothing ->
                        Nothing
        }


view : UIObject -> Html msg
view (UIObject internals) =
    case internals.visible of
        Just vis ->
            case internals.selectable of
                Just s ->
                    if s.selected then
                        Visible.view internals.rect s.view

                    else
                        Visible.view internals.rect vis

                Nothing ->
                    Visible.view internals.rect vis

        Nothing ->
            ViewHelpers.none


containsCoordinate : Coordinate -> UIObject -> Bool
containsCoordinate coordinate (UIObject internals) =
    Rect.containsCoordinate coordinate internals.rect
