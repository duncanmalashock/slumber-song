port module Ports exposing
    ( JsMessage(..)
    , encodeMessageToJs
    , fromJs
    , jsMessageDecoder
    , toJs
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- PORTS


port toJs : Encode.Value -> Cmd msg


port fromJs : (Decode.Value -> msg) -> Sub msg



-- TYPES


type JsMessage
    = Alert String
    | Data String



-- DECODE


jsMessageDecoder : Decoder JsMessage
jsMessageDecoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "alert" ->
                        Decode.map Alert (Decode.field "message" Decode.string)

                    "data" ->
                        Decode.map Data (Decode.field "payload" Decode.string)

                    _ ->
                        Decode.fail "Unknown tag"
            )



-- ENCODE


encodeMessageToJs : JsMessage -> Encode.Value
encodeMessageToJs msg =
    case msg of
        Alert s ->
            Encode.object
                [ ( "tag", Encode.string "alert" )
                , ( "message", Encode.string s )
                ]

        Data s ->
            Encode.object
                [ ( "tag", Encode.string "data" )
                , ( "payload", Encode.string s )
                ]
