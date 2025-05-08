module Vent.Expression exposing (ExpressionBool(..), ExpressionInt(..), ExpressionString(..), decoder, encode, encodeInt, encodeString, evaluate, evaluateInt, evaluateString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Vent.Attribute exposing (Attribute(..))


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


encode : ExpressionBool -> Encode.Value
encode expression =
    case expression of
        LiteralBool b ->
            Encode.object
                [ ( "tag", Encode.string "LiteralBool" )
                , ( "value", Encode.bool b )
                ]

        ExpAttributeBool { objId, key } ->
            Encode.object
                [ ( "tag", Encode.string "ExpAttributeBool" )
                , ( "objId", Encode.string objId )
                , ( "key", Encode.string key )
                ]

        BoolEquals a b ->
            Encode.object
                [ ( "tag", Encode.string "BoolEquals" )
                , ( "left", encode a )
                , ( "right", encode b )
                ]

        IntEquals a b ->
            Encode.object
                [ ( "tag", Encode.string "IntEquals" )
                , ( "left", encodeInt a )
                , ( "right", encodeInt b )
                ]

        IntGreaterThan a b ->
            Encode.object
                [ ( "tag", Encode.string "IntGreaterThan" )
                , ( "left", encodeInt a )
                , ( "right", encodeInt b )
                ]

        IntLessThan a b ->
            Encode.object
                [ ( "tag", Encode.string "IntLessThan" )
                , ( "left", encodeInt a )
                , ( "right", encodeInt b )
                ]

        StringEquals a b ->
            Encode.object
                [ ( "tag", Encode.string "StringEquals" )
                , ( "left", encodeString a )
                , ( "right", encodeString b )
                ]

        StringContains a b ->
            Encode.object
                [ ( "tag", Encode.string "StringContains" )
                , ( "left", encodeString a )
                , ( "right", encodeString b )
                ]

        And a b ->
            Encode.object
                [ ( "tag", Encode.string "And" )
                , ( "left", encode a )
                , ( "right", encode b )
                ]

        Or a b ->
            Encode.object
                [ ( "tag", Encode.string "Or" )
                , ( "left", encode a )
                , ( "right", encode b )
                ]

        Not sub ->
            Encode.object
                [ ( "tag", Encode.string "Not" )
                , ( "expr", encode sub )
                ]


encodeInt : ExpressionInt -> Encode.Value
encodeInt expr =
    case expr of
        LiteralInt i ->
            Encode.object
                [ ( "tag", Encode.string "LiteralInt" )
                , ( "value", Encode.int i )
                ]

        ExpAttributeInt { objId, key } ->
            Encode.object
                [ ( "tag", Encode.string "ExpAttributeInt" )
                , ( "objId", Encode.string objId )
                , ( "key", Encode.string key )
                ]


encodeString : ExpressionString -> Encode.Value
encodeString expr =
    case expr of
        LiteralString s ->
            Encode.object
                [ ( "tag", Encode.string "LiteralString" )
                , ( "value", Encode.string s )
                ]

        ExpAttributeString { objId, key } ->
            Encode.object
                [ ( "tag", Encode.string "ExpAttributeString" )
                , ( "objId", Encode.string objId )
                , ( "key", Encode.string key )
                ]
