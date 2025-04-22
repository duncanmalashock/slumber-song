module AttributeStore exposing (AttributeStore, getById, new)

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
