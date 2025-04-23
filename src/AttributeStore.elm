module AttributeStore exposing (AttributeStore, getById, new, setBool, setInt)

import Attribute exposing (Attribute)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


type AttributeStore
    = AttributeStore Internals


type alias Internals =
    { attributes : Dict String Attribute
    }


new : List ( String, Attribute ) -> AttributeStore
new attributesForInit =
    let
        attributes : Dict String Attribute
        attributes =
            attributesForInit
                |> Dict.fromList
    in
    AttributeStore
        { attributes = attributes
        }


getById : String -> AttributeStore -> Maybe Attribute
getById id (AttributeStore internals) =
    Dict.get id internals.attributes


setInt : { id : String, value : Int } -> AttributeStore -> AttributeStore
setInt { id, value } (AttributeStore internals) =
    AttributeStore
        { attributes =
            Dict.insert id (Attribute.int value) internals.attributes
        }


setBool : { id : String, value : Bool } -> AttributeStore -> AttributeStore
setBool { id, value } (AttributeStore internals) =
    AttributeStore
        { attributes =
            Dict.insert id (Attribute.bool value) internals.attributes
        }
