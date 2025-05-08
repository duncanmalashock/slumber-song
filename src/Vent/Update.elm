module Vent.Update exposing (Update(..), decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Vent.Expression as Expression


type Update
    = ClearSelections
    | IncrementAttribute { objId : String, attributeKey : String, value : Int }
    | SetBoolAttribute { objId : String, attributeKey : String, value : Expression.ExpressionBool }
    | SetIntAttribute { objId : String, attributeKey : String, value : Expression.ExpressionInt }
    | SetStringAttribute { objId : String, attributeKey : String, value : Expression.ExpressionString }


decoder : Decoder Update
decoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "ClearSelections" ->
                        Decode.succeed ClearSelections

                    "IncrementAttribute" ->
                        Decode.map3
                            (\objId attributeKey value ->
                                IncrementAttribute { objId = objId, attributeKey = attributeKey, value = value }
                            )
                            (Decode.field "objId" Decode.string)
                            (Decode.field "attributeKey" Decode.string)
                            (Decode.field "value" Decode.int)

                    "SetBoolAttribute" ->
                        Decode.map3
                            (\objId attributeKey value ->
                                SetBoolAttribute { objId = objId, attributeKey = attributeKey, value = value }
                            )
                            (Decode.field "objId" Decode.string)
                            (Decode.field "attributeKey" Decode.string)
                            (Decode.field "value" Expression.decoder)

                    _ ->
                        Decode.fail ("Unknown update tag: " ++ tag)
            )


encode : Update -> Encode.Value
encode update =
    case update of
        ClearSelections ->
            Encode.object
                [ ( "tag", Encode.string "ClearSelections" )
                ]

        IncrementAttribute { objId, attributeKey, value } ->
            Encode.object
                [ ( "tag", Encode.string "IncrementAttribute" )
                , ( "objId", Encode.string objId )
                , ( "attributeKey", Encode.string attributeKey )
                , ( "value", Encode.int value )
                ]

        SetBoolAttribute { objId, attributeKey, value } ->
            Encode.object
                [ ( "tag", Encode.string "SetBoolAttribute" )
                , ( "objId", Encode.string objId )
                , ( "attributeKey", Encode.string attributeKey )
                , ( "value", Expression.encode value )
                ]

        SetIntAttribute { objId, attributeKey, value } ->
            Encode.object
                [ ( "tag", Encode.string "SetIntAttribute" )
                , ( "objId", Encode.string objId )
                , ( "attributeKey", Encode.string attributeKey )
                , ( "value", Expression.encodeInt value )
                ]

        SetStringAttribute { objId, attributeKey, value } ->
            Encode.object
                [ ( "tag", Encode.string "SetBoolAttribute" )
                , ( "objId", Encode.string objId )
                , ( "attributeKey", Encode.string attributeKey )
                , ( "value", Expression.encodeString value )
                ]
