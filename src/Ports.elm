port module Ports exposing
    ( receive
    , send
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ToJs


send : List ToJs.ToJs -> Cmd msg
send toJsMsgs =
    toJs (ToJs.encodeList toJsMsgs)


receive : (Decode.Value -> msg) -> Sub msg
receive toMsg =
    fromJs toMsg


port toJs : Encode.Value -> Cmd msg


port fromJs : (Decode.Value -> msg) -> Sub msg
