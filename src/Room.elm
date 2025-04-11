module Room exposing (Room, decoder, exits, id, name, new, soundsOnEnter)

import Exit
import Json.Decode as Decode exposing (Decoder)


decoder : Decoder Room
decoder =
    let
        construct : String -> String -> List Exit.Exit -> Maybe String -> Room
        construct myId myName myExits maybeSound =
            let
                onEnter : List String
                onEnter =
                    case maybeSound of
                        Just filename ->
                            [ filename ]

                        Nothing ->
                            []
            in
            Room
                { id = myId
                , name = myName
                , exits = myExits
                , soundsOnEnter = onEnter
                }
    in
    Decode.map4 construct
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "exits" (Decode.list Exit.decoder))
        (Decode.maybe (Decode.field "sound" Decode.string))


type Room
    = Room Internals


type alias Internals =
    { id : String
    , name : String
    , exits : List Exit.Exit
    , soundsOnEnter : List String
    }


new : { id : String, name : String, exits : List Exit.Exit } -> Room
new params =
    Room
        { id = params.id
        , name = params.name
        , exits = params.exits
        , soundsOnEnter = []
        }


id : Room -> String
id (Room internals) =
    internals.id


name : Room -> String
name (Room internals) =
    internals.name


exits : Room -> List Exit.Exit
exits (Room internals) =
    internals.exits


soundsOnEnter : Room -> List String
soundsOnEnter (Room internals) =
    internals.soundsOnEnter
