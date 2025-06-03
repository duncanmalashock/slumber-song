module Vent.GameFile exposing (GameFile, decoder)

import Decode
import Json.Decode as Decode exposing (Decoder)


decoder : Decoder GameFile
decoder =
    let
        constructGameFile : List Room -> List Object -> GameFile
        constructGameFile myRooms myInventory =
            GameFile
                { rooms = myRooms
                , inventory = myInventory
                }
    in
    Decode.map2 constructGameFile
        (Decode.field "rooms" (Decode.list roomDecoder))
        (Decode.field "inventory" (Decode.list objectDecoder))


type GameFile
    = GameFile Internals


type alias Internals =
    { rooms : List Room
    , inventory : List Object
    }


type alias Room =
    { id : String
    , image : String
    , description : String
    , objects : List Object
    }


type alias Object =
    { id : String
    , image : String
    , description : String
    , positionX : Int
    , positionY : Int
    , width : Int
    , height : Int
    }


roomDecoder : Decoder Room
roomDecoder =
    Decode.map4 Room
        (Decode.field "id" Decode.string)
        (Decode.field "image" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "objects" (Decode.list objectDecoder))


objectDecoder : Decoder Object
objectDecoder =
    let
        constructObject :
            String
            -> String
            -> String
            -> Rect
            -> Object
        constructObject myId myImage myDescription myRect =
            { id = myId
            , image = myImage
            , description = myDescription
            , positionX = myRect.x
            , positionY = myRect.y
            , width = myRect.w
            , height = myRect.h
            }
    in
    Decode.map4 constructObject
        (Decode.field "id" Decode.string)
        (Decode.field "image" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "rect" decodeRect)


decodeRect : Decoder Rect
decodeRect =
    Decode.list Decode.int
        |> Decode.andThen listToRectDecoder


listToRectDecoder : List Int -> Decoder Rect
listToRectDecoder intList =
    case intList of
        x :: y :: w :: h :: _ ->
            Decode.succeed
                { x = x
                , y = y
                , w = w
                , h = h
                }

        _ ->
            Decode.fail "Couldn't decode Rect data"


type alias Rect =
    { x : Int
    , y : Int
    , w : Int
    , h : Int
    }
