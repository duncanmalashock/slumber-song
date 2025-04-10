port module Ports exposing
    ( receive
    , send
    )

import FromJs
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ToJs


send : ToJs.ToJs -> Cmd msg
send toJsMsg =
    toJs (ToJs.encode toJsMsg)


receive : (FromJs.FromJs -> msg) -> Sub msg
receive toMsg =
    let
        decodeJsMsg : Decode.Value -> msg
        decodeJsMsg val =
            case Decode.decodeValue FromJs.decoder val of
                Ok msg ->
                    toMsg msg

                Err _ ->
                    toMsg (FromJs.Data "Failed to decode")
    in
    fromJs decodeJsMsg


port toJs : Encode.Value -> Cmd msg


port fromJs : (Decode.Value -> msg) -> Sub msg
