module MacOS.Rect exposing (Rect, addPosition, height, new, posX, posY, size, width)

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
