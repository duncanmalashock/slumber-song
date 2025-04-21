module Condition exposing (Condition, decoder)

import Json.Decode as Decode exposing (Decoder)


type Condition
    = ConditionAny


decoder : Decoder Condition
decoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "ConditionAny" ->
                        Decode.succeed ConditionAny

                    _ ->
                        Decode.fail ("Unknown condition tag: " ++ tag)
            )
