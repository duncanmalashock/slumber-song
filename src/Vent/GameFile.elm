module Vent.GameFile exposing (GameFile, Object, Room, currentRoom, decoder, toObjectList)

import Decode
import Json.Decode as Decode exposing (Decoder)
import Vent.Object


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


toObjectList : GameFile -> List Vent.Object.Object
toObjectList (GameFile internals) =
    List.concat
        [ internals.rooms
            |> List.concatMap
                (\room ->
                    room.objects
                        |> List.map (objectToVentObject room.id)
                )
        , internals.inventory
            |> List.map (objectToVentObject "inventory")
        , internals.rooms
            |> List.map roomToVentObject
        ]


objectToVentObject : String -> Object -> Vent.Object.Object
objectToVentObject parentId object =
    Vent.Object.new
        { id = object.id
        , name = object.name
        , parent = parentId
        , description = object.description
        , attributes = []
        , scripts = []
        }


roomToVentObject : Room -> Vent.Object.Object
roomToVentObject room =
    Vent.Object.new
        { id = room.id
        , name = room.name
        , parent = "world"
        , description = room.description
        , attributes = []
        , scripts = []
        }


currentRoom : GameFile -> Maybe Room
currentRoom (GameFile internals) =
    List.head internals.rooms


type alias Room =
    { id : String
    , name : String
    , image : String
    , description : String
    , objects : List Object
    }


type alias Object =
    { id : String
    , name : String
    , image : String
    , description : String
    , positionX : Int
    , positionY : Int
    , width : Int
    , height : Int
    }


roomDecoder : Decoder Room
roomDecoder =
    Decode.map5 Room
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
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
            -> String
            -> Rect
            -> Object
        constructObject myId myName myImage myDescription myRect =
            { id = myId
            , name = myName
            , image = myImage
            , description = myDescription
            , positionX = myRect.x
            , positionY = myRect.y
            , width = myRect.w
            , height = myRect.h
            }
    in
    Decode.map5 constructObject
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
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
        w :: h :: x :: y :: _ ->
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
