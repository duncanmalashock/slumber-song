module Room exposing (Room, exits, id, name, new)

import Exit


type Room
    = Room Internals


type alias Internals =
    { id : String
    , name : String
    , exits : List Exit.Exit
    }


new : { id : String, name : String, exits : List Exit.Exit } -> Room
new params =
    Room
        { id = params.id
        , name = params.name
        , exits = params.exits
        }


id : Room -> String
id (Room internals) =
    internals.id


name : Room -> String
name (Room internals) =
    internals.name


exits : Room -> List Exit.Exit
exits (Room internals) =
    internals.exits
