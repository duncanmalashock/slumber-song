module Effect exposing (Effect(..), encodeList)

import Command exposing (Command)
import Json.Encode as Encode


type Effect
    = LoadGameData String
    | PlaySound String
    | HighlightCommand Command
    | HighlightObject String
    | PrintText String
    | ReportError String


encodeList : List Effect -> Encode.Value
encodeList effects =
    Encode.list encode effects


encode : Effect -> Encode.Value
encode effect =
    case effect of
        LoadGameData file ->
            Encode.object
                [ ( "tag", Encode.string "LoadGameData" )
                , ( "file", Encode.string file )
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

        HighlightObject objId ->
            Encode.object
                [ ( "tag", Encode.string "HighlightObject" )
                , ( "objId", Encode.string objId )
                ]

        PrintText text ->
            Encode.object
                [ ( "tag", Encode.string "PrintText" )
                , ( "text", Encode.string text )
                ]

        ReportError message ->
            Encode.object
                [ ( "tag", Encode.string "ReportError" )
                , ( "message", Encode.string message )
                ]
