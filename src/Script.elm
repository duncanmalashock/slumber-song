module Script exposing (Script, decoder, run)

import Command exposing (Command)
import Effect exposing (Effect)
import Expression exposing (Expression)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Trigger exposing (Trigger)
import Update exposing (Update)


type alias Script =
    { trigger : Trigger
    , condition : Expression
    , updates : List Update
    , effects : List Effect
    }


decoder : Decoder Script
decoder =
    Decode.succeed Script
        |> required "trigger" Trigger.decoder
        |> required "condition" Expression.decoder
        |> required "updates" (Decode.list Update.decoder)
        |> required "effects" (Decode.list Effect.decoder)


run : Script -> ( List Update, List Effect )
run script =
    if Trigger.shouldRun script.trigger then
        if Expression.evaluate script.condition then
            ( script.updates, script.effects )

        else
            ( [], [] )

    else
        ( [], [] )
