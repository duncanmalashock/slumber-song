module Vent.Effect exposing (Effect(..), decoder, encode, encodeList)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Vent.Command exposing (Command)


type Effect
    = LoadGameData String
    | PlaySound String


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

                    _ ->
                        Decode.fail ("Unknown effect tag: " ++ tag)
            )
