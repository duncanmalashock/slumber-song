module Script exposing (Script, decoder, encode)

import Command exposing (Command)
import Effect exposing (Effect)
import Expression exposing (ExpressionBool)
import Interaction exposing (Interaction(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
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


encode : Script -> Encode.Value
encode script =
    Encode.object
        [ ( "trigger", Trigger.encode script.trigger )
        , ( "condition", Expression.encode script.condition )
        , ( "updates", Encode.list Update.encode script.updates )
        , ( "effects", Encode.list Effect.encode script.effects )
        ]
