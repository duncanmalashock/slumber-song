module Update exposing (Update(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type Update
    = ClearSelections
    | AddToAttribute { objId : String, attributeKey : String, value : Int }


decoder : Decoder Update
decoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "ClearSelections" ->
                        Decode.succeed ClearSelections

                    "AddToAttribute" ->
                        Decode.map3
                            (\objId attributeKey value ->
                                AddToAttribute { objId = objId, attributeKey = attributeKey, value = value }
                            )
                            (Decode.field "objId" Decode.string)
                            (Decode.field "attributeKey" Decode.string)
                            (Decode.field "value" Decode.int)

                    _ ->
                        Decode.fail ("Unknown update tag: " ++ tag)
            )
