module Vent.AttributeStore exposing (AttributeStore, encode, getById, new, setBool, setInt, setString)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Vent.Attribute as Attribute exposing (Attribute)


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


setString : { id : String, value : String } -> AttributeStore -> AttributeStore
setString { id, value } (AttributeStore internals) =
    AttributeStore
        { attributes =
            Dict.insert id (Attribute.string value) internals.attributes
        }


toDict : AttributeStore -> Dict String Attribute
toDict (AttributeStore internals) =
    internals.attributes


encode : AttributeStore -> Encode.Value
encode store =
    toDict store
        |> Dict.toList
        |> List.map (\( k, v ) -> ( k, Attribute.encode v ))
        |> Encode.object


decoder : Decoder AttributeStore
decoder =
    Decode.dict Attribute.decoder
        |> Decode.map (\dict -> AttributeStore { attributes = dict })
