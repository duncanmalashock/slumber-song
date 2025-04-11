module FromJs exposing (FromJs(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type FromJs
    = UserClickedGoButton
    | UserClickedExit String
    | DecodeError Decode.Error


decoder : Decoder FromJs
decoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "userClickedGoButton" ->
                        Decode.succeed UserClickedGoButton

                    "userClickedExit" ->
                        Decode.field "payload"
                            (Decode.map UserClickedExit (Decode.field "toRoomId" Decode.string))

                    _ ->
                        Decode.fail "Unknown tag"
            )
