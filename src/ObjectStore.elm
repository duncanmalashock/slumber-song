module ObjectStore exposing (ObjectStore, getById, new, withParentId)

import Dict exposing (Dict)
import Object exposing (Object)


type ObjectStore
    = ObjectStore Internals


type alias Internals =
    { objects : Dict String Object
    }


new : List Object -> ObjectStore
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
    ObjectStore
        { objects = objects
        }


getById : String -> ObjectStore -> Object
getById id (ObjectStore internals) =
    Dict.get id internals.objects
        |> Maybe.withDefault Object.null


withParentId : String -> ObjectStore -> List Object
withParentId id (ObjectStore internals) =
    Dict.toList internals.objects
        |> List.filter
            (\( key, obj ) ->
                Object.parent obj == id
            )
        |> List.map (\( key, obj ) -> obj)
