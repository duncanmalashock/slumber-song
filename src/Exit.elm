module Exit exposing (Exit, decoder, new, toRoomId)

import Json.Decode as Decode exposing (Decoder)


type Exit
    = Exit Internals


type alias Internals =
    { toRoomId : String
    }


new : { toRoomId : String } -> Exit
new params =
    Exit
        { toRoomId = params.toRoomId
        }


toRoomId : Exit -> String
toRoomId (Exit internals) =
    internals.toRoomId


decoder : Decoder Exit
decoder =
    Decode.field "toRoomId" Decode.string
        |> Decode.map
            (\id ->
                Exit
                    { toRoomId = id
                    }
            )
