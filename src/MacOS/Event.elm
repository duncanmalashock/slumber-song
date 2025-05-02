module MacOS.Event exposing (Event(..), Registry, eventToMsgList, on, registry)

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


on : ObjectId -> Event -> msg -> Registry msg -> Registry msg
on objectId event msg (Registry dict) =
    let
        append : Maybe (List msg) -> Maybe (List msg)
        append maybeList =
            case maybeList of
                Nothing ->
                    Just [ msg ]

                Just list ->
                    Just (msg :: list)

        updateEventDict : Maybe (Dict EventId (List msg)) -> Maybe (Dict EventId (List msg))
        updateEventDict maybeDict =
            Just
                (Dict.update
                    (eventToString event)
                    append
                    (Maybe.withDefault Dict.empty maybeDict)
                )
    in
    Registry (Dict.update objectId updateEventDict dict)
