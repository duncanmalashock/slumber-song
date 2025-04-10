module Room exposing (Room, id, name, new)

import Exit


type Room
    = Room Internals


type alias Internals =
    { id : String
    , name : String
    , exits : List Exit.Exit
    }


new : { id : String, name : String } -> Room
new params =
    Room
        { id = params.id
        , name = params.name
        , exits = []
        }


id : Room -> String
id (Room internals) =
    internals.id


name : Room -> String
name (Room internals) =
    internals.name
