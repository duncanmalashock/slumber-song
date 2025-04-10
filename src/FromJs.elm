module FromJs exposing (FromJs(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type FromJs
    = Data String


decoder : Decoder FromJs
decoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen decodePayload


decodePayload : String -> Decoder FromJs
decodePayload tag =
    case tag of
        "data" ->
            Decode.map Data (Decode.field "payload" Decode.string)

        _ ->
            Decode.fail "Unknown tag"
