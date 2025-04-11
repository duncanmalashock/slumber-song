port module Ports exposing
    ( receive
    , send
    )

import FromJs
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ToJs


send : List ToJs.ToJs -> Cmd msg
send toJsMsgs =
    toJs (ToJs.encodeList toJsMsgs)


receive : (FromJs.FromJs -> msg) -> Sub msg
receive toMsg =
    let
        decodeJsMsg : Decode.Value -> msg
        decodeJsMsg val =
            case Decode.decodeValue FromJs.decoder val of
                Ok msg ->
                    toMsg msg

                Err err ->
                    toMsg (FromJs.DecodeError err)
    in
    fromJs decodeJsMsg


port toJs : Encode.Value -> Cmd msg


port fromJs : (Decode.Value -> msg) -> Sub msg
