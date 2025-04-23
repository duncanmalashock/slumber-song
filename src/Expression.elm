module Expression exposing (ExpressionBool(..), decoder, evaluate)

import Attribute exposing (Attribute(..))
import Json.Decode as Decode exposing (Decoder)


type ExpressionBool
    = LiteralBool Bool
    | ExpAttributeBool { objId : String, key : String }
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
    | ExpAttributeInt { objId : String, key : String }


type ExpressionString
    = LiteralString String
    | ExpAttributeString { objId : String, key : String }


type alias LookupFn =
    { objectId : String, attributeId : String } -> Maybe Attribute


evaluate : LookupFn -> ExpressionBool -> Bool
evaluate lookup expression =
    case expression of
        LiteralBool b ->
            b

        ExpAttributeBool record ->
            case lookup { objectId = record.objId, attributeId = record.key } of
                Just (AttributeBool val) ->
                    val

                _ ->
                    False

        -- default if not found or wrong type
        BoolEquals a b ->
            evaluate lookup a == evaluate lookup b

        IntEquals a b ->
            evaluateInt lookup a == evaluateInt lookup b

        IntGreaterThan a b ->
            evaluateInt lookup a > evaluateInt lookup b

        IntLessThan a b ->
            evaluateInt lookup a < evaluateInt lookup b

        StringEquals a b ->
            evaluateString lookup a == evaluateString lookup b

        StringContains a b ->
            String.contains (evaluateString lookup b) (evaluateString lookup a)

        And a b ->
            evaluate lookup a && evaluate lookup b

        Or a b ->
            evaluate lookup a || evaluate lookup b

        Not sub ->
            not (evaluate lookup sub)


evaluateInt : LookupFn -> ExpressionInt -> Int
evaluateInt lookup expression =
    case expression of
        LiteralInt i ->
            i

        ExpAttributeInt record ->
            case lookup { objectId = record.objId, attributeId = record.key } of
                Just (AttributeInt val) ->
                    val

                _ ->
                    0


evaluateString : LookupFn -> ExpressionString -> String
evaluateString lookup expression =
    case expression of
        LiteralString s ->
            s

        ExpAttributeString record ->
            case lookup { objectId = record.objId, attributeId = record.key } of
                Just (AttributeString val) ->
                    val

                _ ->
                    ""


decoder : Decoder ExpressionBool
decoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen decodeByTag


decodeByTag : String -> Decoder ExpressionBool
decodeByTag tag =
    case tag of
        "LiteralBool" ->
            Decode.map LiteralBool (Decode.field "value" Decode.bool)

        "ExpAttributeBool" ->
            Decode.map ExpAttributeBool fieldDecoder

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

                    "ExpAttributeInt" ->
                        Decode.map ExpAttributeInt fieldDecoder

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

                    "ExpAttributeString" ->
                        Decode.map ExpAttributeString fieldDecoder

                    _ ->
                        Decode.fail ("Unknown string tag: " ++ tag)
            )


fieldDecoder : Decoder { objId : String, key : String }
fieldDecoder =
    Decode.map2 (\objId key -> { objId = objId, key = key })
        (Decode.field "objId" Decode.string)
        (Decode.field "key" Decode.string)
