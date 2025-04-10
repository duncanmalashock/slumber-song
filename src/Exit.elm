module Exit exposing (Exit)


type Exit
    = Exit Internals


type alias Internals =
    { toRoomId : String
    }
