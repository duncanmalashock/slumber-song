module Trigger exposing (Trigger, decoder, shouldRun)

import Command exposing (Command)
import Interaction exposing (Interaction(..))
import Json.Decode as Decode exposing (Decoder)


type Trigger
    = OnAny
    | OnCommand Command


shouldRun : String -> Trigger -> Interaction -> Bool
shouldRun objectIdToMatch trigger interaction =
    case trigger of
        OnAny ->
            True

        OnCommand command ->
            Interaction.handlesCommand command interaction
                && Interaction.handlesObject objectIdToMatch interaction


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
