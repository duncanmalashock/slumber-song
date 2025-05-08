module Vent.Attribute exposing (Attribute(..), bool, decoder, encode, int, string)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Attribute
    = AttributeBool Bool
    | AttributeInt Int
    | AttributeString String


bool : Bool -> Attribute
bool value =
    AttributeBool value


int : Int -> Attribute
int value =
    AttributeInt value


string : String -> Attribute
string value =
    AttributeString value


decoder : Decoder Attribute
decoder =
    Decode.oneOf
        [ Decode.bool |> Decode.map AttributeBool
        , Decode.int |> Decode.map AttributeInt
        , Decode.string |> Decode.map AttributeString
        ]


encode : Attribute -> Encode.Value
encode attr =
    case attr of
        AttributeBool b ->
            Encode.bool b

        AttributeInt i ->
            Encode.int i

        AttributeString s ->
            Encode.string s
