module Trigger exposing (Trigger, decoder)

import Command exposing (Command)
import Json.Decode as Decode exposing (Decoder)


type Trigger
    = OnAny
    | OnCommand Command


decoder : Decoder Trigger
decoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "OnAny" ->
                        Decode.succeed OnAny

                    "OnCommand" ->
                        Decode.field "command" Command.decoder
                            |> Decode.map OnCommand

                    _ ->
                        Decode.fail ("Unknown trigger tag: " ++ tag)
            )
