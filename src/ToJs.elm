module ToJs exposing (ToJs(..), encode)

import Exit
import Json.Encode as Encode
import Room


type ToJs
    = RoomChanged Room.Room


encode : ToJs -> Encode.Value
encode msg =
    case msg of
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
