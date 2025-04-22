module Expression exposing (ExpressionBool(..), decoder, evaluate)

import Json.Decode as Decode exposing (Decoder)


type ExpressionBool
    = LiteralBool Bool
    | AttributeBool { objId : String, key : String }
    | BoolEquals ExpressionBool ExpressionBool
    | IntEquals ExpressionInt ExpressionInt
    | IntGreaterThan ExpressionInt ExpressionInt
    | IntLessThan ExpressionInt ExpressionInt
    | StringEquals ExpressionString ExpressionString
    | StringContains ExpressionString ExpressionString
    | And ExpressionBool ExpressionBool
    | Or ExpressionBool ExpressionBool
    | Not ExpressionBool


type ExpressionInt
    = LiteralInt Int
    | AttributeInt { objId : String, key : String }


type ExpressionString
    = LiteralString String
    | AttributeString { objId : String, key : String }


evaluate : ExpressionBool -> Bool
evaluate expression =
    True


decoder : Decoder ExpressionBool
decoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen decodeByTag


decodeByTag : String -> Decoder ExpressionBool
decodeByTag tag =
    case tag of
        "LiteralBool" ->
            Decode.map LiteralBool (Decode.field "value" Decode.bool)

        "AttributeBool" ->
            Decode.map AttributeBool fieldDecoder

        "BoolEquals" ->
            Decode.map2 BoolEquals
                (Decode.field "left" decoder)
                (Decode.field "right" decoder)

        "IntEquals" ->
            Decode.map2 IntEquals
                (Decode.field "left" decodeExpressionInt)
                (Decode.field "right" decodeExpressionInt)

        "IntGreaterThan" ->
            Decode.map2 IntGreaterThan
                (Decode.field "left" decodeExpressionInt)
                (Decode.field "right" decodeExpressionInt)

        "IntLessThan" ->
            Decode.map2 IntLessThan
                (Decode.field "left" decodeExpressionInt)
                (Decode.field "right" decodeExpressionInt)

        "StringEquals" ->
            Decode.map2 StringEquals
                (Decode.field "left" decodeExpressionString)
                (Decode.field "right" decodeExpressionString)

        "StringContains" ->
            Decode.map2 StringContains
                (Decode.field "left" decodeExpressionString)
                (Decode.field "right" decodeExpressionString)

        "And" ->
            Decode.map2 And
                (Decode.field "left" decoder)
                (Decode.field "right" decoder)

        "Or" ->
            Decode.map2 Or
                (Decode.field "left" decoder)
                (Decode.field "right" decoder)

        "Not" ->
            Decode.map Not (Decode.field "expr" decoder)

        _ ->
            Decode.fail ("Unknown tag: " ++ tag)


decodeExpressionInt : Decoder ExpressionInt
decodeExpressionInt =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "LiteralInt" ->
                        Decode.map LiteralInt (Decode.field "value" Decode.int)

                    "AttributeInt" ->
                        Decode.map AttributeInt fieldDecoder

                    _ ->
                        Decode.fail ("Unknown int tag: " ++ tag)
            )


decodeExpressionString : Decoder ExpressionString
decodeExpressionString =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "LiteralString" ->
                        Decode.map LiteralString (Decode.field "value" Decode.string)

                    "AttributeString" ->
                        Decode.map AttributeString fieldDecoder

                    _ ->
                        Decode.fail ("Unknown string tag: " ++ tag)
            )


fieldDecoder : Decoder { objId : String, key : String }
fieldDecoder =
    Decode.map2 (\objId key -> { objId = objId, key = key })
        (Decode.field "objId" Decode.string)
        (Decode.field "key" Decode.string)
