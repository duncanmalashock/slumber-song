module ToJs exposing (ToJs(..), encodeList)

import Command exposing (Command)
import Exit
import Json.Encode as Encode
import Room


type ToJs
    = LoadGameData
    | UpdateRoom Room.Room
    | PlaySound String
    | HighlightCommand Command


encodeList : List ToJs -> Encode.Value
encodeList toJsMsgs =
    Encode.list encode toJsMsgs


encode : ToJs -> Encode.Value
encode toJsMsg =
    case toJsMsg of
        LoadGameData ->
            Encode.object
                [ ( "tag", Encode.string "LoadGameData" )
                ]

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

        HighlightCommand command ->
            Encode.object
                [ ( "tag", Encode.string "HighlightCommand" )
                , ( "command", Encode.string (Command.toString command) )
                ]
