module MacOS.UI.Object exposing
    ( Object, new
    , image, textarea
    , setView, setSelectOptions, setDragOptions
    , onClick, onDoubleClick, onDragStart, onMouseDown
    , id, position, size, text, rect
    , getDragOptions, getMouseEventHandler
    , setPosition, setRect, setText, setSelected
    , addPosition
    , view
    )

{-| A part of the UI, where it exists in the context of a program's interface, and how it can be interacted with.


# Object

@docs Object, new


## Kinds of Objects

@docs image, textarea


# Define interactions

@docs setView, setSelectOptions, setDragOptions
@docs onClick, onDoubleClick, onDragStart, onMouseDown


# Query

@docs id, position, size, text, rect
@docs getDragOptions, getMouseEventHandler


# Update

@docs setPosition, setRect, setText, setSelected
@docs addPosition


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
import MacOS.UI.View.Textarea as Textarea


type Object msg
    = Object (Internals msg)


type alias Internals msg =
    { id : String
    , rect : Rect
    , text : String
    , view : Maybe (View msg)
    , selectOptions : Maybe (SelectOptions msg)
    , dragOptions : Maybe (DragOptions msg)
    , onMouseDown : Maybe msg
    , onClick : Maybe msg
    , onDoubleClick : Maybe msg
    , onDragStart : Maybe msg
    }


type alias DragOptions msg =
    { traveling : View msg }


type alias SelectOptions msg =
    { view : View msg
    , selected : Bool
    }


new : { id : String, rect : Rect } -> Object msg
new params =
    Object
        { id = params.id
        , rect = params.rect
        , text = ""
        , view = Nothing
        , selectOptions = Nothing
        , dragOptions = Nothing
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
        , text = ""
        , view = Just (View.image { url = params.url, size = params.size })
        , selectOptions = Nothing
        , dragOptions = Nothing
        , onMouseDown = Nothing
        , onClick = Nothing
        , onDoubleClick = Nothing
        , onDragStart = Nothing
        }


textarea : { id : String, font : Textarea.Font, color : Textarea.Color } -> Object msg
textarea params =
    Object
        { id = params.id
        , rect = Rect.new ( 0, 0 ) ( 0, 0 )
        , text = ""
        , view = Just (View.textarea { color = params.color, font = params.font })
        , selectOptions = Nothing
        , dragOptions = Nothing
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


setView : View msg -> Object msg -> Object msg
setView objectView (Object internals) =
    Object
        { internals
            | view = Just objectView
        }


setSelectOptions : SelectOptions msg -> Object msg -> Object msg
setSelectOptions objectView (Object internals) =
    Object
        { internals
            | selectOptions = Just objectView
        }


setDragOptions : DragOptions msg -> Object msg -> Object msg
setDragOptions params (Object internals) =
    Object
        { internals
            | dragOptions =
                Just
                    { traveling = params.traveling
                    }
        }


getDragOptions : Object msg -> Maybe (DragOptions msg)
getDragOptions (Object internals) =
    internals.dragOptions


rect : Object msg -> Rect
rect (Object internals) =
    internals.rect


text : Object msg -> String
text (Object internals) =
    internals.text


size : Object msg -> Coordinate
size (Object internals) =
    Rect.size internals.rect


position : Object msg -> Coordinate
position (Object internals) =
    Rect.position internals.rect


setPosition : Coordinate -> Object msg -> Object msg
setPosition newValue (Object internals) =
    Object
        { internals
            | rect = Rect.setPosition newValue internals.rect
        }


addPosition : Coordinate -> Object msg -> Object msg
addPosition newValue (Object internals) =
    Object
        { internals
            | rect = Rect.plus newValue internals.rect
        }


setRect : Rect -> Object msg -> Object msg
setRect newValue (Object internals) =
    Object
        { internals
            | rect = newValue
        }


setText : String -> Object msg -> Object msg
setText newValue (Object internals) =
    Object
        { internals
            | text = newValue
        }


setSelected : Bool -> Object msg -> Object msg
setSelected newValue (Object internals) =
    Object
        { internals
            | selectOptions =
                case internals.selectOptions of
                    Just s ->
                        Just
                            { s
                                | selected = newValue
                            }

                    Nothing ->
                        Nothing
        }


view : { debug : Bool } -> Object msg -> List (Html msg) -> Html msg
view { debug } (Object internals) childrenViews =
    div
        [ Html.Attributes.id internals.id
        , if debug then
            UIHelpers.debugColorize

          else
            style "" ""
        ]
        [ case internals.view of
            Just objectView ->
                case internals.selectOptions of
                    Just s ->
                        if s.selected then
                            View.view internals.rect internals.text s.view childrenViews

                        else
                            View.view internals.rect internals.text objectView childrenViews

                    Nothing ->
                        View.view internals.rect internals.text objectView childrenViews

            Nothing ->
                div [ Html.Attributes.class "NO_VIEW" ]
                    childrenViews
        ]
