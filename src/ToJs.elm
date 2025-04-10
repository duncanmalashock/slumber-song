module ToJs exposing (ToJs(..), encode)

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
                ]
