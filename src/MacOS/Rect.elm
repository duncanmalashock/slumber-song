module MacOS.Rect exposing (Rect, bottom, containsCoordinate, height, left, new, plus, posX, posY, position, right, setPosition, size, top, width)

import MacOS.Coordinate as Coordinate exposing (Coordinate)


type Rect
    = Rect { position : Coordinate, size : Coordinate }


new : ( Int, Int ) -> ( Int, Int ) -> Rect
new ( x, y ) ( w, h ) =
    Rect
        { position = Coordinate.new ( x, y )
        , size = Coordinate.new ( w, h )
        }


position : Rect -> Coordinate
position (Rect internals) =
    internals.position


setPosition : Coordinate -> Rect -> Rect
setPosition newValue (Rect internals) =
    Rect
        { internals
            | position = newValue
        }


containsCoordinate : Coordinate -> Rect -> Bool
containsCoordinate coordinate rect =
    let
        px : Int
        px =
            Coordinate.x coordinate

        py : Int
        py =
            Coordinate.y coordinate
    in
    (px >= left rect)
        && (px <= right rect)
        && (py >= top rect)
        && (py <= bottom rect)


posX : Rect -> Int
posX (Rect internals) =
    Coordinate.x internals.position


posY : Rect -> Int
posY (Rect internals) =
    Coordinate.y internals.position


plus : Coordinate -> Rect -> Rect
plus coordinate (Rect internals) =
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
