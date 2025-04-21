module Object exposing (Object, decoder, description, id, name, new, null, parent)

import Attribute exposing (Attribute)
import Attributes exposing (Attributes)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Script exposing (Script)


type Object
    = Object Internals


type alias Internals =
    { id : String
    , name : String
    , parent : String
    , description : String
    , attributes : Attributes
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
                        |> Attributes.new
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
        , attributes = Attributes.new params.attributes
        , scripts = params.scripts
        }


null : Object
null =
    Object
        { id = ""
        , name = ""
        , parent = ""
        , description = ""
        , attributes = Attributes.new []
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
