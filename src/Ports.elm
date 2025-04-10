port module Ports exposing
    ( fromJs
    , toJs
    )

import FromJs
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ToJs


port toJs : Encode.Value -> Cmd msg


port fromJs : (Decode.Value -> msg) -> Sub msg
