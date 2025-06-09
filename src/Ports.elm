port module Ports exposing
    ( loadGame
    , receive
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


loadGame : { filename : String } -> Cmd msg
loadGame params =
    Encode.object
        [ ( "tag", Encode.string "LoadGameData" )
        , ( "file", Encode.string params.filename )
        ]
        |> toJs


receive : (Decode.Value -> msg) -> Sub msg
receive toMsg =
    fromJs toMsg


port toJs : Encode.Value -> Cmd msg


port fromJs : (Decode.Value -> msg) -> Sub msg
