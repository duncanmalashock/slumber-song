module Vent.Object exposing (Object, attribute, decoder, description, encode, id, image, immovable, name, null, parent, rect, scripts, setBoolAttribute, setIntAttribute, setStringAttribute)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline
import Json.Encode as Encode
import MacOS.Rect as Rect exposing (Rect)
import Vent.Attribute as Attribute exposing (Attribute)
import Vent.AttributeStore as AttributeStore exposing (AttributeStore)
import Vent.VentScript.Script as Script exposing (Script)


type Object
    = Object Internals


type alias Internals =
    { id : String
    , objectType : ObjectType
    , name : String
    , parent : String
    , rect : Rect
    , image : String
    , description : String
    , immovable : Bool
    , attributes : AttributeStore
    , scripts : List Script
    }


type ObjectType
    = World
    | Player
    | Room
    | Door
    | Generic


decoder : Decoder Object
decoder =
    let
        construct :
            String
            -> ObjectType
            -> String
            -> String
            -> Rect
            -> String
            -> String
            -> Bool
            -> Dict String Attribute
            -> List Script
            -> Object
        construct myId myType myName myParent myRect myImage myDescription myImmovable myAttributes myScripts =
            Object
                { id = myId
                , objectType = myType
                , name = myName
                , parent = myParent
                , rect = myRect
                , image = myImage
                , description = myDescription
                , immovable = myImmovable
                , attributes =
                    myAttributes
                        |> AttributeStore.new
                , scripts = myScripts
                }
    in
    Decode.succeed construct
        |> Json.Decode.Pipeline.required "id" Decode.string
        |> Json.Decode.Pipeline.required "type" objectTypeDecoder
        |> Json.Decode.Pipeline.required "name" Decode.string
        |> Json.Decode.Pipeline.required "parent" Decode.string
        |> Json.Decode.Pipeline.required "rect" Rect.decoder
        |> Json.Decode.Pipeline.required "image" Decode.string
        |> Json.Decode.Pipeline.required "description" Decode.string
        |> Json.Decode.Pipeline.required "immovable" Decode.bool
        |> Json.Decode.Pipeline.required "attributes" (Decode.dict Attribute.decoder)
        |> Json.Decode.Pipeline.required "scripts" (Decode.list Script.decoder)


objectTypeDecoder : Decoder ObjectType
objectTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "world" ->
                        Decode.succeed World

                    "player" ->
                        Decode.succeed Player

                    "room" ->
                        Decode.succeed Room

                    "door" ->
                        Decode.succeed Door

                    "generic" ->
                        Decode.succeed Generic

                    otherValue ->
                        Decode.fail ("Couldn't decode objectType: " ++ otherValue)
            )


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
    , objectType : ObjectType
    , name : String
    , parent : String
    , rect : Rect
    , image : String
    , description : String
    , immovable : Bool
    , attributes : Dict String Attribute
    , scripts : List Script
    }
    -> Object
new params =
    Object
        { id = params.id
        , objectType = params.objectType
        , name = params.name
        , parent = params.parent
        , rect = params.rect
        , image = params.image
        , description = params.description
        , immovable = params.immovable
        , attributes = AttributeStore.new params.attributes
        , scripts = params.scripts
        }


null : Object
null =
    Object
        { id = ""
        , objectType = Generic
        , name = ""
        , parent = ""
        , rect = Rect.new ( 0, 0 ) ( 0, 0 )
        , image = ""
        , description = ""
        , immovable = False
        , attributes = AttributeStore.new Dict.empty
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


image : Object -> String
image (Object internals) =
    internals.image


rect : Object -> Rect
rect (Object internals) =
    internals.rect


immovable : Object -> Bool
immovable (Object internals) =
    internals.immovable


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
