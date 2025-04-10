module Exit exposing (Exit, new, toRoomId)


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
