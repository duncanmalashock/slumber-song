module MacOS.Event exposing (Event(..), Registry, eventToMsgList, registerObject, registry)

import Dict exposing (Dict)


type Registry msg
    = Registry (Internals msg)


type alias Internals msg =
    Dict ObjectId (Dict EventId (List msg))


type alias EventId =
    String


type alias ObjectId =
    String


type Event
    = DoubleClick
    | Click


eventToString : Event -> String
eventToString event =
    case event of
        DoubleClick ->
            "DoubleClick"

        Click ->
            "Click"


registry : Registry msg
registry =
    Registry Dict.empty


eventToMsgList : ObjectId -> Event -> Registry msg -> List msg
eventToMsgList objectId event (Registry dict) =
    case Dict.get objectId dict of
        Just eventDict ->
            Dict.get (eventToString event) eventDict
                |> Maybe.withDefault []

        Nothing ->
            []


registerObject : ObjectId -> List { on : Event, msg : msg } -> Registry msg -> Registry msg
registerObject objectId handlers (Registry dict) =
    List.foldl (on objectId) (Registry dict) handlers


on : ObjectId -> { on : Event, msg : msg } -> Registry msg -> Registry msg
on objectId handler (Registry dict) =
    let
        append : Maybe (List msg) -> Maybe (List msg)
        append maybeList =
            case maybeList of
                Nothing ->
                    Just [ handler.msg ]

                Just list ->
                    Just (handler.msg :: list)

        updateEventDict : Maybe (Dict EventId (List msg)) -> Maybe (Dict EventId (List msg))
        updateEventDict maybeDict =
            Just
                (Dict.update
                    (eventToString handler.on)
                    append
                    (Maybe.withDefault Dict.empty maybeDict)
                )
    in
    Registry (Dict.update objectId updateEventDict dict)
