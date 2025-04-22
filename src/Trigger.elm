module Trigger exposing (Trigger, decoder, shouldRun)

import Command exposing (Command)
import Interaction exposing (Interaction(..))
import Json.Decode as Decode exposing (Decoder)


type Trigger
    = OnAny
    | OnCommand Command
    | OnMove


shouldRun : Trigger -> Interaction -> Bool
shouldRun trigger interaction =
    case trigger of
        OnAny ->
            True

        OnMove ->
            case interaction of
                AttemptMoveObject _ ->
                    True

                _ ->
                    False

        OnCommand command ->
            Interaction.matchesCommand command interaction


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

                    "OnMove" ->
                        Decode.succeed OnMove

                    _ ->
                        Decode.fail ("Unknown trigger tag: " ++ tag)
            )
