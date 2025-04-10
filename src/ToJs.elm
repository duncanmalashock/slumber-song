module ToJs exposing (ToJs(..), encode)

import Json.Encode as Encode


type ToJs
    = Alert String
    | Data String


encode : ToJs -> Encode.Value
encode msg =
    case msg of
        Alert s ->
            Encode.object
                [ ( "tag", Encode.string "alert" )
                , ( "message", Encode.string s )
                ]

        Data s ->
            Encode.object
                [ ( "tag", Encode.string "data" )
                , ( "payload", Encode.string s )
                ]
