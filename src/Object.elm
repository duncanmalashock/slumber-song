module Object exposing (Object, decoder, description, id, name, new, null, parent)

import Attribute exposing (Attribute)
import Attributes exposing (Attributes)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


type Object
    = Object Internals


type alias Internals =
    { id : String
    , name : String
    , parent : String
    , description : String
    , attributes : Attributes
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
            -> Object
        construct myId myName myParent myDescription myAttributes =
            Object
                { id = myId
                , name = myName
                , parent = myParent
                , description = myDescription
                , attributes =
                    myAttributes
                        |> Dict.toList
                        |> Attributes.new
                }
    in
    Decode.map5 construct
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "parent" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "attributes" (Decode.dict Attribute.decoder))


new :
    { id : String
    , name : String
    , parent : String
    , description : String
    , attributes : List ( String, Attribute )
    }
    -> Object
new params =
    Object
        { id = params.id
        , name = params.name
        , parent = params.parent
        , description = params.description
        , attributes = Attributes.new params.attributes
        }


null : Object
null =
    Object
        { id = ""
        , name = ""
        , parent = ""
        , description = ""
        , attributes = Attributes.new []
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
