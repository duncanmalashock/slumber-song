module Map exposing (Map, getRoomById, new)

import Dict exposing (Dict)
import Room exposing (Room)


type Map
    = Map Internals


type alias Internals =
    { rooms : Dict String Room
    }


new : List Room -> Map
new roomsForInit =
    let
        rooms : Dict String Room
        rooms =
            roomsForInit
                |> List.map
                    (\r ->
                        ( Room.id r, r )
                    )
                |> Dict.fromList
    in
    Map
        { rooms = rooms
        }


getRoomById : String -> Map -> Maybe Room
getRoomById id (Map internals) =
    Dict.get id internals.rooms
