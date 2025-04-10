port module Ports exposing
    ( encodeMessageToJs
    , fromJs
    , jsMessageDecoder
    , toJs
    )

import FromJs
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ToJs



-- PORTS


port toJs : Encode.Value -> Cmd msg


port fromJs : (Decode.Value -> msg) -> Sub msg



-- TYPES
-- DECODE


jsMessageDecoder : Decoder FromJs.FromJs
jsMessageDecoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "alert" ->
                        Decode.map FromJs.Alert (Decode.field "message" Decode.string)

                    "data" ->
                        Decode.map FromJs.Data (Decode.field "payload" Decode.string)

                    _ ->
                        Decode.fail "Unknown tag"
            )



-- ENCODE


encodeMessageToJs : ToJs.ToJs -> Encode.Value
encodeMessageToJs msg =
    case msg of
        ToJs.Alert s ->
            Encode.object
                [ ( "tag", Encode.string "alert" )
                , ( "message", Encode.string s )
                ]

        ToJs.Data s ->
            Encode.object
                [ ( "tag", Encode.string "data" )
                , ( "payload", Encode.string s )
                ]
