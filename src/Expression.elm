module Expression exposing (Expression(..), decoder, evaluate)

import Json.Decode as Decode exposing (Decoder)


type Expression
    = LiteralBool Bool
    | LiteralInt Int
    | LiteralString String
    | AttributeBool { objId : String, attributeKey : String }
    | AttributeInt { objId : String, attributeKey : String }
    | AttributeString { objId : String, attributeKey : String }
    | Equals Expression Expression
    | GreaterThan Expression Expression
    | LessThan Expression Expression
    | Contains Expression Expression
    | And Expression Expression
    | Or Expression Expression
    | Not Expression


decoder : Decoder Expression
decoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen decodeByTag


decodeByTag : String -> Decoder Expression
decodeByTag tag =
    case tag of
        "LiteralBool" ->
            Decode.field "value" Decode.bool
                |> Decode.map LiteralBool

        "LiteralInt" ->
            Decode.field "value" Decode.int
                |> Decode.map LiteralInt

        "LiteralString" ->
            Decode.field "value" Decode.string
                |> Decode.map LiteralString

        "AttributeBool" ->
            Decode.map2 (\objId key -> AttributeBool { objId = objId, attributeKey = key })
                (Decode.field "objId" Decode.string)
                (Decode.field "attributeKey" Decode.string)

        "AttributeInt" ->
            Decode.map2 (\objId key -> AttributeInt { objId = objId, attributeKey = key })
                (Decode.field "objId" Decode.string)
                (Decode.field "attributeKey" Decode.string)

        "AttributeString" ->
            Decode.map2 (\objId key -> AttributeString { objId = objId, attributeKey = key })
                (Decode.field "objId" Decode.string)
                (Decode.field "attributeKey" Decode.string)

        "Equals" ->
            Decode.map2 Equals
                (Decode.field "left" decoder)
                (Decode.field "right" decoder)

        "GreaterThan" ->
            Decode.map2 GreaterThan
                (Decode.field "left" decoder)
                (Decode.field "right" decoder)

        "LessThan" ->
            Decode.map2 LessThan
                (Decode.field "left" decoder)
                (Decode.field "right" decoder)

        "Contains" ->
            Decode.map2 Contains
                (Decode.field "left" decoder)
                (Decode.field "right" decoder)

        "And" ->
            Decode.map2 And
                (Decode.field "left" decoder)
                (Decode.field "right" decoder)

        "Or" ->
            Decode.map2 Or
                (Decode.field "left" decoder)
                (Decode.field "right" decoder)

        "Not" ->
            Decode.field "expression" decoder
                |> Decode.map Not

        _ ->
            Decode.fail ("Unknown expression tag: " ++ tag)


evaluate : Expression -> Bool
evaluate expression =
    True
