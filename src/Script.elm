module Script exposing (Script, decoder)

import Command exposing (Command)
import Effect exposing (Effect)
import Expression exposing (Expression)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Trigger exposing (Trigger)
import Update exposing (Update)


type alias Script =
    { on : Trigger
    , condition : Expression
    , updates : List Update
    , effects : List Effect
    }


decoder : Decoder Script
decoder =
    Decode.succeed Script
        |> required "on" Trigger.decoder
        |> required "condition" Expression.decoder
        |> required "updates" (Decode.list Update.decoder)
        |> required "effects" (Decode.list Effect.decoder)
