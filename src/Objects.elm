module Objects exposing (Objects, getById, new, withParentId)

import Dict exposing (Dict)
import Object exposing (Object)


type Objects
    = Objects Internals


type alias Internals =
    { objects : Dict String Object
    }


new : List Object -> Objects
new objectsForInit =
    let
        objects : Dict String Object
        objects =
            objectsForInit
                |> List.map
                    (\r ->
                        ( Object.id r, r )
                    )
                |> Dict.fromList
    in
    Objects
        { objects = objects
        }


getById : String -> Objects -> Maybe Object
getById id (Objects internals) =
    Dict.get id internals.objects


withParentId : String -> Objects -> List Object
withParentId id (Objects internals) =
    Dict.toList internals.objects
        |> List.filter
            (\( key, obj ) ->
                Object.parent obj == id
            )
        |> List.map (\( key, obj ) -> obj)
