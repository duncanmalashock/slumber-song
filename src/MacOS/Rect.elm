module MacOS.Rect exposing (Rect, bottom, containsCoordinate, height, interpolate, left, new, plus, posX, posY, position, right, setPosition, size, top, width)

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


interpolate : Rect -> Rect -> Float -> Rect
interpolate (Rect r1) (Rect r2) percent =
    let
        interpolateInt : Int -> Int -> Int
        interpolateInt a b =
            round <| toFloat a + percent * (toFloat b - toFloat a)

        pos1 =
            r1.position

        pos2 =
            r2.position

        size1 =
            r1.size

        size2 =
            r2.size

        interpX =
            interpolateInt (Coordinate.x pos1) (Coordinate.x pos2)

        interpY =
            interpolateInt (Coordinate.y pos1) (Coordinate.y pos2)

        interpW =
            interpolateInt (Coordinate.x size1) (Coordinate.x size2)

        interpH =
            interpolateInt (Coordinate.y size1) (Coordinate.y size2)
    in
    Rect
        { position = Coordinate.new ( interpX, interpY )
        , size = Coordinate.new ( interpW, interpH )
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
