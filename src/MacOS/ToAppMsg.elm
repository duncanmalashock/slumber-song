module MacOS.ToAppMsg exposing (DroppedUIObjectInfo, ToAppMsg(..))

import MacOS.Rect as Rect exposing (Rect)


type ToAppMsg
    = DroppedUIObject DroppedUIObjectInfo
    | DoubleClickedUIObject DoubleClickedUIObjectInfo


type alias ObjectId =
    String


type alias DroppedUIObjectInfo =
    { objectId : ObjectId
    , isWindow : Bool
    , droppedOnWindow : Maybe ObjectId
    , droppedOnObjects : List ObjectId
    , dropRectAbsolute : Rect
    , dropRectInWindow : Rect
    , originRect : Rect
    }


type alias DoubleClickedUIObjectInfo =
    { objectId : ObjectId
    }
