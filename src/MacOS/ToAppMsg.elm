module MacOS.ToAppMsg exposing (DroppedObjectInfo, ToAppMsg(..))

import MacOS.Coordinate as Coordinate exposing (Coordinate)


type ToAppMsg
    = DroppedObject DroppedObjectInfo


type alias DroppedObjectInfo =
    { objectId : ObjectId
    , droppedOnWindow : Maybe ObjectId
    , droppedOnObjects : List ObjectId
    , dropPositionAbsolute : Coordinate
    , dropPositionInWindow : Coordinate
    }


type alias ObjectId =
    String
