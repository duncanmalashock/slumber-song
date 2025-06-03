port module Ports exposing
    ( loadGame
    , receive
    , send
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Vent.Effect as Effect exposing (Effect)


loadGame : { filename : String } -> Cmd msg
loadGame params =
    Encode.object
        [ ( "tag", Encode.string "LoadGameData" )
        , ( "file", Encode.string params.filename )
        ]
        |> toJs


send : List Effect -> Cmd msg
send effects =
    toJs (Effect.encodeList effects)


receive : (Decode.Value -> msg) -> Sub msg
receive toMsg =
    fromJs toMsg


port toJs : Encode.Value -> Cmd msg


port fromJs : (Decode.Value -> msg) -> Sub msg
