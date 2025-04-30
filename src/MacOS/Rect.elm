module MacOS.Rect exposing (Rect, addPosition, bottom, height, left, new, posX, posY, right, size, top, width)

import MacOS.Coordinate as Coordinate exposing (Coordinate)


type Rect
    = Rect { position : Coordinate, size : Coordinate }


new : ( Int, Int ) -> ( Int, Int ) -> Rect
new ( x, y ) ( w, h ) =
    Rect
        { position = Coordinate.new ( x, y )
        , size = Coordinate.new ( w, h )
        }


posX : Rect -> Int
posX (Rect { position }) =
    Coordinate.x position


posY : Rect -> Int
posY (Rect { position }) =
    Coordinate.y position


addPosition : Coordinate -> Rect -> Rect
addPosition coordinate (Rect internals) =
    Rect
        { internals
            | position = Coordinate.plus coordinate internals.position
        }


width : Rect -> Int
width (Rect internals) =
    Coordinate.x internals.size


height : Rect -> Int
height (Rect internals) =
    Coordinate.y internals.size


size : Rect -> Coordinate
size (Rect internals) =
    internals.size


top : Rect -> Int
top (Rect internals) =
    Coordinate.y internals.position


bottom : Rect -> Int
bottom (Rect internals) =
    Coordinate.y internals.position
        + Coordinate.y internals.size


left : Rect -> Int
left (Rect internals) =
    Coordinate.x internals.position


right : Rect -> Int
right (Rect internals) =
    Coordinate.x internals.position
        + Coordinate.x internals.size
