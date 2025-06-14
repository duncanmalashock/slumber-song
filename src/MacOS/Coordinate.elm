module MacOS.Coordinate exposing
    ( Coordinate, new
    , x, y
    , plus, minus
    , cityBlockDistance
    , toTuple
    )

{-| A point in screen space.


# Coordinate

@docs Coordinate, new


# Query

@docs x, y


# Coordinate math

@docs plus, minus
@docs cityBlockDistance


# Convert

@docs toTuple

-}


type Coordinate
    = Coordinate { x : Int, y : Int }


new : ( Int, Int ) -> Coordinate
new ( newX, newY ) =
    Coordinate { x = newX, y = newY }


x : Coordinate -> Int
x (Coordinate p) =
    p.x


y : Coordinate -> Int
y (Coordinate p) =
    p.y


plus : Coordinate -> Coordinate -> Coordinate
plus (Coordinate p1) (Coordinate p2) =
    Coordinate { x = p1.x + p2.x, y = p1.y + p2.y }


minus : Coordinate -> Coordinate -> Coordinate
minus (Coordinate p1) (Coordinate p2) =
    Coordinate { x = p2.x - p1.x, y = p2.y - p1.y }


cityBlockDistance : Coordinate -> Coordinate -> Int
cityBlockDistance (Coordinate p1) (Coordinate p2) =
    Basics.abs (p1.x - p2.x) + Basics.abs (p1.y - p2.y)


toTuple : Coordinate -> ( Int, Int )
toTuple (Coordinate p) =
    ( p.x, p.y )
