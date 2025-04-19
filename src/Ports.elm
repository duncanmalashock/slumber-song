port module Ports exposing
    ( receive
    , send
    )

import Effect exposing (Effect)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


send : List Effect -> Cmd msg
send effects =
    toJs (Effect.encodeList effects)


receive : (Decode.Value -> msg) -> Sub msg
receive toMsg =
    fromJs toMsg


port toJs : Encode.Value -> Cmd msg


port fromJs : (Decode.Value -> msg) -> Sub msg
