module Script exposing (Script, decoder, run)

import Command exposing (Command)
import Effect exposing (Effect)
import Expression exposing (ExpressionBool)
import Interaction exposing (Interaction(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Trigger exposing (Trigger)
import Update exposing (Update)


type alias Script =
    { trigger : Trigger
    , condition : ExpressionBool
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


run : Interaction -> Script -> ( List Update, List Effect )
run interaction script =
    if Trigger.shouldRun script.trigger interaction then
        if Expression.evaluate script.condition then
            ( script.updates, script.effects )

        else
            ( [], [] )

    else
        ( [], [] )
