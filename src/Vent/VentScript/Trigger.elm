module Vent.VentScript.Trigger exposing (Trigger(..), decoder, encode, fromString, shouldRun)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Vent.Command as Command exposing (Command)
import Vent.Interaction as Interaction exposing (Interaction(..))


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


fromString : String -> Result String Trigger
fromString string =
    case string of
        "any" ->
            Ok OnAny

        _ ->
            Command.fromString string
                |> Result.map OnCommand


encode : Trigger -> Encode.Value
encode trigger =
    case trigger of
        OnAny ->
            Encode.object
                [ ( "tag", Encode.string "OnAny" )
                ]

        OnCommand cmd ->
            Encode.object
                [ ( "tag", Encode.string "OnCommand" )
                , ( "command", Encode.string (Command.toString cmd) )
                ]
