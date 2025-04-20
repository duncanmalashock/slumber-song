module Object exposing (Object, decoder, id, name, new)

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
    , attributes : Attributes
    }


decoder : Decoder Object
decoder =
    let
        construct :
            String
            -> String
            -> String
            -> Dict String Attribute
            -> Object
        construct myId myName myParent myAttributes =
            Object
                { id = myId
                , name = myName
                , parent = myParent
                , attributes =
                    myAttributes
                        |> Dict.toList
                        |> Attributes.new
                }
    in
    Decode.map4 construct
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "parent" Decode.string)
        (Decode.field "attributes" (Decode.dict Attribute.decoder))


new :
    { id : String
    , name : String
    , parent : String
    , attributes : List ( String, Attribute )
    }
    -> Object
new params =
    Object
        { id = params.id
        , name = params.name
        , parent = params.parent
        , attributes = Attributes.new params.attributes
        }


id : Object -> String
id (Object internals) =
    internals.id


name : Object -> String
name (Object internals) =
    internals.name
