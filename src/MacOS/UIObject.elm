module MacOS.UIObject exposing
    ( UIObject
    , containsCoordinate
    , draggable
    , getDraggable
    , getMouseEventHandler
    , new
    , onClick
    , onDoubleClick
    , onDragStart
    , onMouseDown
    , position
    , rect
    , selectable
    , setPosition
    , setRect
    , setSelected
    , view
    , visible
    )

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Mouse as Mouse
import MacOS.Rect as Rect exposing (Rect)
import MacOS.ViewHelpers as ViewHelpers
import MacOS.Visible as Visible exposing (Visible)


type UIObject msg
    = UIObject (Internals msg)


type alias Internals msg =
    { rect : Rect
    , visible : Maybe (Visible msg)
    , selectable : Maybe (Selectable msg)
    , draggable : Maybe (Draggable msg)
    , onMouseDown : Maybe msg
    , onClick : Maybe msg
    , onDoubleClick : Maybe msg
    , onDragStart : Maybe msg
    }


type alias Draggable msg =
    { traveling : Visible msg }


type alias Selectable msg =
    { view : Visible msg
    , selected : Bool
    }


new : { rect : Rect } -> UIObject msg
new params =
    UIObject
        { rect = params.rect
        , visible = Nothing
        , selectable = Nothing
        , draggable = Nothing
        , onMouseDown = Nothing
        , onClick = Nothing
        , onDoubleClick = Nothing
        , onDragStart = Nothing
        }


onMouseDown : msg -> UIObject msg -> UIObject msg
onMouseDown msg (UIObject internals) =
    UIObject
        { internals
            | onMouseDown = Just msg
        }


onClick : msg -> UIObject msg -> UIObject msg
onClick msg (UIObject internals) =
    UIObject
        { internals
            | onClick = Just msg
        }


onDoubleClick : msg -> UIObject msg -> UIObject msg
onDoubleClick msg (UIObject internals) =
    UIObject
        { internals
            | onDoubleClick = Just msg
        }


onDragStart : msg -> UIObject msg -> UIObject msg
onDragStart msg (UIObject internals) =
    UIObject
        { internals
            | onDragStart = Just msg
        }


getMouseEventHandler : Mouse.Event -> UIObject msg -> Maybe msg
getMouseEventHandler mouseEvent (UIObject internals) =
    case mouseEvent of
        Mouse.MouseDown _ ->
            internals.onMouseDown

        Mouse.MouseUp ->
            Nothing

        Mouse.Click _ ->
            internals.onClick

        Mouse.DoubleClick _ ->
            internals.onDoubleClick

        Mouse.DragStart _ ->
            internals.onDragStart


visible : Visible msg -> UIObject msg -> UIObject msg
visible vis (UIObject internals) =
    UIObject
        { internals
            | visible = Just vis
        }


selectable : Selectable msg -> UIObject msg -> UIObject msg
selectable vis (UIObject internals) =
    UIObject
        { internals
            | selectable = Just vis
        }


draggable : Draggable msg -> UIObject msg -> UIObject msg
draggable params (UIObject internals) =
    UIObject
        { internals
            | draggable =
                Just
                    { traveling = params.traveling
                    }
        }


getDraggable : UIObject msg -> Maybe (Draggable msg)
getDraggable (UIObject internals) =
    internals.draggable


rect : UIObject msg -> Rect
rect (UIObject internals) =
    internals.rect


position : UIObject msg -> Coordinate
position (UIObject internals) =
    Rect.position internals.rect


setPosition : Coordinate -> UIObject msg -> UIObject msg
setPosition newValue (UIObject internals) =
    UIObject
        { internals
            | rect = Rect.setPosition newValue internals.rect
        }


setRect : Rect -> UIObject msg -> UIObject msg
setRect newValue (UIObject internals) =
    UIObject
        { internals
            | rect = newValue
        }


setSelected : Bool -> UIObject msg -> UIObject msg
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


view : UIObject msg -> Html msg
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


containsCoordinate : Coordinate -> UIObject msg -> Bool
containsCoordinate coordinate (UIObject internals) =
    Rect.containsCoordinate coordinate internals.rect
