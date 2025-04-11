module ToJs exposing (ToJs(..), encodeList)

import Exit
import Json.Encode as Encode
import Room


type ToJs
    = UpdateRoom Room.Room
    | PlaySound String


encodeList : List ToJs -> Encode.Value
encodeList toJsMsgs =
    Encode.list encode toJsMsgs


encode : ToJs -> Encode.Value
encode toJsMsg =
    case toJsMsg of
        UpdateRoom newRoom ->
            Encode.object
                [ ( "tag", Encode.string "UpdateRoom" )
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

        PlaySound file ->
            Encode.object
                [ ( "tag", Encode.string "PlaySound" )
                , ( "file", Encode.string ("/game/sfx/" ++ file ++ ".mp3") )
                ]
