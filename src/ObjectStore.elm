module ObjectStore exposing (ObjectStore, getAttribute, getById, incrementAttributeBy, new, setBoolAttribute, withParentId)

import Attribute exposing (Attribute(..))
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


getAttribute : ObjectStore -> { objectId : String, attributeId : String } -> Maybe Attribute
getAttribute objectStore { objectId, attributeId } =
    getById objectId objectStore
        |> Object.attribute attributeId


incrementAttributeBy : ObjectStore -> { objectId : String, attributeId : String, amount : Int } -> ObjectStore
incrementAttributeBy ((ObjectStore internals) as objectStore) { objectId, attributeId, amount } =
    case getAttribute objectStore { objectId = objectId, attributeId = attributeId } of
        Just (AttributeInt intValue) ->
            ObjectStore
                { internals
                    | objects =
                        Dict.insert objectId
                            (Object.setIntAttribute
                                { id = attributeId, value = intValue + amount }
                                (getById objectId objectStore)
                            )
                            internals.objects
                }

        _ ->
            objectStore


setBoolAttribute : ObjectStore -> { objectId : String, attributeId : String, value : Bool } -> ObjectStore
setBoolAttribute ((ObjectStore internals) as objectStore) { objectId, attributeId, value } =
    case getAttribute objectStore { objectId = objectId, attributeId = attributeId } of
        Just (AttributeBool intValue) ->
            ObjectStore
                { internals
                    | objects =
                        Dict.insert objectId
                            (Object.setBoolAttribute
                                { id = attributeId, value = value }
                                (getById objectId objectStore)
                            )
                            internals.objects
                }

        _ ->
            objectStore
