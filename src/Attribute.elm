module Attribute exposing (Attribute(..), bool, decoder, int, string)

import Json.Decode as Decode exposing (Decoder)


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
