module Attributes exposing (Attributes, getById, new)

import Attribute exposing (Attribute)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


type Attributes
    = Attributes Internals


type alias Internals =
    { attributes : Dict String Attribute
    }


new : List ( String, Attribute ) -> Attributes
new attributesForInit =
    let
        attributes : Dict String Attribute
        attributes =
            attributesForInit
                |> Dict.fromList
    in
    Attributes
        { attributes = attributes
        }


getById : String -> Attributes -> Maybe Attribute
getById id (Attributes internals) =
    Dict.get id internals.attributes
