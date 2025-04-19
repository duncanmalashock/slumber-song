module Effect exposing (Effect(..), encodeList)

import Command exposing (Command)
import Exit
import Json.Encode as Encode
import Room


type Effect
    = LoadGameData
    | UpdateRoom Room.Room
    | PlaySound String
    | HighlightCommand Command
    | ReportError String


encodeList : List Effect -> Encode.Value
encodeList effects =
    Encode.list encode effects


encode : Effect -> Encode.Value
encode effect =
    case effect of
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

        ReportError message ->
            Encode.object
                [ ( "tag", Encode.string "ReportError" )
                , ( "message", Encode.string message )
                ]
