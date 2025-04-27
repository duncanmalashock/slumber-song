module ObjectStore exposing (ObjectStore, get, getAttribute, getNoFail, idExists, incrementAttributeBy, new, setBoolAttribute, toList, withParentId)

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


toList : ObjectStore -> List Object
toList (ObjectStore internals) =
    Dict.toList internals.objects
        |> List.map Tuple.second


get : String -> ObjectStore -> Maybe Object
get id (ObjectStore internals) =
    Dict.get id internals.objects


idExists : String -> ObjectStore -> Bool
idExists id (ObjectStore internals) =
    case Dict.get id internals.objects of
        Just _ ->
            True

        Nothing ->
            False


getNoFail : String -> ObjectStore -> Object
getNoFail id (ObjectStore internals) =
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
    getNoFail objectId objectStore
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
                                (getNoFail objectId objectStore)
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
                                (getNoFail objectId objectStore)
                            )
                            internals.objects
                }

        _ ->
            objectStore
