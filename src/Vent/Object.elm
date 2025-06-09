module Vent.Object exposing (Object, attribute, decoder, description, encode, id, name, new, null, parent, scripts, setBoolAttribute, setIntAttribute, setStringAttribute)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Vent.Attribute as Attribute exposing (Attribute)
import Vent.AttributeStore as AttributeStore exposing (AttributeStore)
import Vent.VentScript.Script as Script exposing (Script)


type Object
    = Object Internals


type alias Internals =
    { id : String
    , name : String
    , parent : String
    , description : String
    , attributes : AttributeStore
    , scripts : List Script
    }


decoder : Decoder Object
decoder =
    let
        construct :
            String
            -> String
            -> String
            -> String
            -> Dict String Attribute
            -> List Script
            -> Object
        construct myId myName myParent myDescription myAttributes myScripts =
            Object
                { id = myId
                , name = myName
                , parent = myParent
                , description = myDescription
                , attributes =
                    myAttributes
                        |> Dict.toList
                        |> AttributeStore.new
                , scripts = myScripts
                }
    in
    Decode.map6 construct
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "parent" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "attributes" (Decode.dict Attribute.decoder))
        (Decode.field "scripts" (Decode.list Script.decoder))


encode : Object -> Encode.Value
encode (Object internals) =
    Encode.object
        [ ( "id", Encode.string internals.id )
        , ( "name", Encode.string internals.name )
        , ( "parent", Encode.string internals.parent )
        , ( "description", Encode.string internals.description )
        , ( "attributes", AttributeStore.encode internals.attributes )
        , ( "scripts", Encode.list Script.encode internals.scripts )
        ]


new :
    { id : String
    , name : String
    , parent : String
    , description : String
    , attributes : List ( String, Attribute )
    , scripts : List Script
    }
    -> Object
new params =
    Object
        { id = params.id
        , name = params.name
        , parent = params.parent
        , description = params.description
        , attributes = AttributeStore.new params.attributes
        , scripts = params.scripts
        }


null : Object
null =
    Object
        { id = ""
        , name = ""
        , parent = ""
        , description = ""
        , attributes = AttributeStore.new []
        , scripts = []
        }


id : Object -> String
id (Object internals) =
    internals.id


name : Object -> String
name (Object internals) =
    internals.name


parent : Object -> String
parent (Object internals) =
    internals.parent


description : Object -> String
description (Object internals) =
    internals.description


attribute : String -> Object -> Maybe Attribute
attribute attributeId (Object internals) =
    internals.attributes
        |> AttributeStore.getById attributeId


setIntAttribute : { id : String, value : Int } -> Object -> Object
setIntAttribute params (Object internals) =
    Object
        { internals
            | attributes =
                internals.attributes
                    |> AttributeStore.setInt { id = params.id, value = params.value }
        }


setBoolAttribute : { id : String, value : Bool } -> Object -> Object
setBoolAttribute params (Object internals) =
    Object
        { internals
            | attributes =
                internals.attributes
                    |> AttributeStore.setBool { id = params.id, value = params.value }
        }


setStringAttribute : { id : String, value : String } -> Object -> Object
setStringAttribute params (Object internals) =
    Object
        { internals
            | attributes =
                internals.attributes
                    |> AttributeStore.setString { id = params.id, value = params.value }
        }


scripts : Object -> List Script
scripts (Object internals) =
    internals.scripts
