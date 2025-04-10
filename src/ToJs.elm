module ToJs exposing (ToJs(..), encode)

import Json.Encode as Encode


type ToJs
    = Alert String


encode : ToJs -> Encode.Value
encode msg =
    case msg of
        Alert s ->
            Encode.object
                [ ( "tag", Encode.string "alert" )
                , ( "message", Encode.string s )
                ]
