module MacOS.UI.Object exposing
    ( Object, new
    , image
    , visible, selectable, draggable
    , onClick, onDoubleClick, onDragStart, onMouseDown
    , id, position, rect
    , getDraggable, getMouseEventHandler
    , containsCoordinate
    , setPosition, setRect, setSelected
    , view
    )

{-| A part of the UI, where it exists in the context of a program's interface, and how it can be interacted with.


# Object

@docs Object, new


## Kinds of Objects

@docs image


# Define interactions

@docs visible, selectable, draggable
@docs onClick, onDoubleClick, onDragStart, onMouseDown


# Query

@docs id, position, rect
@docs getDraggable, getMouseEventHandler


# Hit testing

@docs containsCoordinate


# Update

@docs setPosition, setRect, setSelected


# View

@docs view

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Mouse as Mouse
import MacOS.Rect as Rect exposing (Rect)
import MacOS.UI.Helpers as UIHelpers
import MacOS.UI.View as View exposing (View)


type Object msg
    = Object (Internals msg)


type alias Internals msg =
    { id : String
    , rect : Rect
    , visible : Maybe (View msg)
    , selectable : Maybe (Selectable msg)
    , draggable : Maybe (Draggable msg)
    , onMouseDown : Maybe msg
    , onClick : Maybe msg
    , onDoubleClick : Maybe msg
    , onDragStart : Maybe msg
    }


type alias Draggable msg =
    { traveling : View msg }


type alias Selectable msg =
    { view : View msg
    , selected : Bool
    }


new : { id : String, rect : Rect } -> Object msg
new params =
    Object
        { id = params.id
        , rect = params.rect
        , visible = Nothing
        , selectable = Nothing
        , draggable = Nothing
        , onMouseDown = Nothing
        , onClick = Nothing
        , onDoubleClick = Nothing
        , onDragStart = Nothing
        }


image : { id : String, url : String, size : ( Int, Int ) } -> Object msg
image params =
    Object
        { id = params.id
        , rect = Rect.new ( 0, 0 ) params.size
        , visible = Just (View.image { url = params.url, size = params.size })
        , selectable = Nothing
        , draggable = Nothing
        , onMouseDown = Nothing
        , onClick = Nothing
        , onDoubleClick = Nothing
        , onDragStart = Nothing
        }


id : Object msg -> String
id (Object internals) =
    internals.id


onMouseDown : msg -> Object msg -> Object msg
onMouseDown msg (Object internals) =
    Object
        { internals
            | onMouseDown = Just msg
        }


onClick : msg -> Object msg -> Object msg
onClick msg (Object internals) =
    Object
        { internals
            | onClick = Just msg
        }


onDoubleClick : msg -> Object msg -> Object msg
onDoubleClick msg (Object internals) =
    Object
        { internals
            | onDoubleClick = Just msg
        }


onDragStart : msg -> Object msg -> Object msg
onDragStart msg (Object internals) =
    Object
        { internals
            | onDragStart = Just msg
        }


getMouseEventHandler : Mouse.Event -> Object msg -> Maybe msg
getMouseEventHandler mouseEvent (Object internals) =
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


visible : View msg -> Object msg -> Object msg
visible vis (Object internals) =
    Object
        { internals
            | visible = Just vis
        }


selectable : Selectable msg -> Object msg -> Object msg
selectable vis (Object internals) =
    Object
        { internals
            | selectable = Just vis
        }


draggable : Draggable msg -> Object msg -> Object msg
draggable params (Object internals) =
    Object
        { internals
            | draggable =
                Just
                    { traveling = params.traveling
                    }
        }


getDraggable : Object msg -> Maybe (Draggable msg)
getDraggable (Object internals) =
    internals.draggable


rect : Object msg -> Rect
rect (Object internals) =
    internals.rect


position : Object msg -> Coordinate
position (Object internals) =
    Rect.position internals.rect


setPosition : Coordinate -> Object msg -> Object msg
setPosition newValue (Object internals) =
    Object
        { internals
            | rect = Rect.setPosition newValue internals.rect
        }


setRect : Rect -> Object msg -> Object msg
setRect newValue (Object internals) =
    Object
        { internals
            | rect = newValue
        }


setSelected : Bool -> Object msg -> Object msg
setSelected newValue (Object internals) =
    Object
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


view : Object msg -> Html msg
view (Object internals) =
    div [ Html.Attributes.id internals.id ]
        [ case internals.visible of
            Just vis ->
                case internals.selectable of
                    Just s ->
                        if s.selected then
                            View.view internals.rect s.view

                        else
                            View.view internals.rect vis

                    Nothing ->
                        View.view internals.rect vis

            Nothing ->
                div [ Html.Attributes.class "NO_VIEW" ] []
        ]


containsCoordinate : Coordinate -> Object msg -> Bool
containsCoordinate coordinate (Object internals) =
    Rect.containsCoordinate coordinate internals.rect
