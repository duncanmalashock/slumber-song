module Decode exposing (log)

import Json.Decode as Decode


log : String -> Decode.Decoder a -> Decode.Decoder a
log message =
    Decode.map (Debug.log message)
