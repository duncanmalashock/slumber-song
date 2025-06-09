module Vent.VentScript.Script exposing (Script, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Vent.Command exposing (Command)
import Vent.Interaction exposing (Interaction(..))
import Vent.VentScript.Expression as Expression exposing (ExpressionBool)
import Vent.VentScript.Statement as Statement exposing (Statement)
import Vent.VentScript.Trigger as Trigger exposing (Trigger)


type alias Script =
    { trigger : Trigger
    , condition : ExpressionBool
    , statements : List Statement
    }


decoder : Decoder Script
decoder =
    Decode.succeed Script
        |> required "trigger" Trigger.decoder
        |> required "condition" Expression.decoder
        |> required "statements" (Decode.list Statement.decoder)


encode : Script -> Encode.Value
encode script =
    Encode.object
        [ ( "trigger", Trigger.encode script.trigger )
        , ( "condition", Expression.encode script.condition )
        , ( "statements", Encode.list Statement.encode script.statements )
        ]
