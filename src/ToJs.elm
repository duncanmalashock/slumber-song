module ToJs exposing (ToJs(..), encodeList)

import Exit
import Json.Encode as Encode
import Room


type ToJs
    = RoomChanged Room.Room


encodeList : List ToJs -> Encode.Value
encodeList toJsMsgs =
    Encode.list encode toJsMsgs


encode : ToJs -> Encode.Value
encode toJsMsg =
    case toJsMsg of
        RoomChanged newRoom ->
            Encode.object
                [ ( "tag", Encode.string "roomChanged" )
                , ( "id", Encode.string (Room.id newRoom) )
                , ( "name", Encode.string (Room.name newRoom) )
                , ( "exits"
                  , Encode.list
                        (\exit ->
                            Encode.object
                                [ ( "toRoomId", Encode.string (Exit.toRoomId exit) )
                                ]
                        )
                        (Room.exits newRoom)
                  )
                ]
