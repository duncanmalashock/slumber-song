module Vent.Effect exposing (Effect(..), decoder, encode, encodeList)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Vent.Command exposing (Command)


type Effect
    = LoadGameData String
    | PlaySound String
    | HighlightCommand Command
    | HighlightObject String
    | PrintText String
    | ReportError String
    | SaveGameData Encode.Value


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
                , ( "file", Encode.string file )
                ]

        HighlightCommand command ->
            Encode.object
                [ ( "tag", Encode.string "HighlightCommand" )
                , ( "command", Encode.string (Vent.Command.toString command) )
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

        SaveGameData gameData ->
            Encode.object
                [ ( "tag", Encode.string "SaveGameData" )
                , ( "gameData", gameData )
                ]


decoder : Decoder Effect
decoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "LoadGameData" ->
                        Decode.field "file" Decode.string
                            |> Decode.map LoadGameData

                    "PlaySound" ->
                        Decode.field "file" Decode.string
                            |> Decode.map PlaySound

                    "HighlightCommand" ->
                        Decode.field "command" Vent.Command.decoder
                            |> Decode.map HighlightCommand

                    "HighlightObject" ->
                        Decode.field "objId" Decode.string
                            |> Decode.map HighlightObject

                    "PrintText" ->
                        Decode.field "text" Decode.string
                            |> Decode.map PrintText

                    "ReportError" ->
                        Decode.field "message" Decode.string
                            |> Decode.map ReportError

                    _ ->
                        Decode.fail ("Unknown effect tag: " ++ tag)
            )
