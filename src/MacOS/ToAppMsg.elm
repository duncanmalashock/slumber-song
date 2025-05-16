module MacOS.ToAppMsg exposing (DroppedObjectInfo, ToAppMsg(..))

import MacOS.Rect as Rect exposing (Rect)


type ToAppMsg
    = DroppedObject DroppedObjectInfo


type alias DroppedObjectInfo =
    { objectId : ObjectId
    , isWindow : Bool
    , droppedOnWindow : Maybe ObjectId
    , droppedOnObjects : List ObjectId
    , dropRectAbsolute : Rect
    , dropRectInWindow : Rect
    , originRect : Rect
    }


type alias ObjectId =
    String
