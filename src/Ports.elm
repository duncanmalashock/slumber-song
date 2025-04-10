module Ports exposing
    ( FromJs(..)
    , ToJs(..)
    , decodeFromJs
    , encodeToJs
    )

import Json.Decode as Decode
import Json.Encode as Encode



-- Msg type you use over the wire


type FromJs
    = Alert String
    | Data String


type ToJs
    = UpdatedRoom



-- Decode message from JS


decodeFromJs : Decode.Value -> Result String FromJs
decodeFromJs val =
    case Decode.decodeValue decoder val of
        Ok fromJs ->
            Ok fromJs

        Err _ ->
            Err "Failed to decode"


decoder : Decode.Decoder FromJs
decoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "alert" ->
                        Decode.map Alert (Decode.field "message" Decode.string)

                    "data" ->
                        Decode.map Data (Decode.field "payload" Decode.string)

                    _ ->
                        Decode.fail "Unknown tag"
            )



-- Encode message to send to JS


encodeToJs : ToJs -> Encode.Value
encodeToJs toJs =
    case toJs of
        UpdatedRoom ->
            Encode.object
                [ ( "tag", Encode.string "alert" )
                , ( "message", Encode.string "I am cool" )
                ]
