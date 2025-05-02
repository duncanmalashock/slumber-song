module MacOS.Mouse exposing (Event(..), Mouse, Msg, Object, debugEvents, eventsForDesktop, new, onMouseDownForObject, onMouseUpForObject, update)

import Html exposing (Attribute)
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Rect exposing (Rect)
import MacOS.Screen as Screen exposing (Screen)
import MacOS.ViewHelpers as ViewHelpers
import Time


type Mouse
    = Mouse Internals


type alias Internals =
    { position : Coordinate
    , msgHistory : List Msg
    , eventHistory : List Event
    , doubleClickTimingThreshold : Int
    }


type Event
    = Click Time.Posix Object
    | DoubleClick Time.Posix Object
    | DragStart Time.Posix Object
    | DragEnd Time.Posix Object


debugEvents : Mouse -> String
debugEvents (Mouse internals) =
    let
        eventToString : Event -> String
        eventToString event =
            case event of
                Click _ { id } ->
                    "Click " ++ id

                DoubleClick _ { id } ->
                    "DoubleClick " ++ id

                DragStart _ { id } ->
                    "DragStart " ++ id

                DragEnd _ { id } ->
                    "DragEnd " ++ id
    in
    internals.eventHistory
        |> List.take 5
        |> List.map eventToString
        |> String.join ", "


new : Mouse
new =
    Mouse
        { position = Coordinate.new ( 0, 0 )
        , msgHistory = []
        , eventHistory = []
        , doubleClickTimingThreshold = 500
        }


type Msg
    = MouseMoved Time.Posix Coordinate
    | MouseUp Time.Posix Object
    | MouseDown Time.Posix Object


type alias Object =
    { id : String
    , offsetFromObjectOrigin : Coordinate
    , mousePosition : Coordinate
    }


maxMsgListLength : Int
maxMsgListLength =
    32


update : Msg -> Mouse -> ( Mouse, List Event )
update msg (Mouse internals) =
    let
        updatedMsgHistory : List Msg
        updatedMsgHistory =
            (msg :: internals.msgHistory)
                |> List.take maxMsgListLength

        foldEvents :
            (List Msg -> List Event -> List Event)
            -> List Event
            -> List Event
        foldEvents detectorFn newEventsSoFar =
            detectorFn updatedMsgHistory (newEventsSoFar ++ internals.eventHistory) ++ newEventsSoFar

        newEventList : List Event
        newEventList =
            List.foldl foldEvents
                []
                [ detectClickEvents
                , detectDragStartEvents
                , detectDragEndEvents
                , detectDoubleClickEvents internals.doubleClickTimingThreshold
                ]
    in
    ( Mouse
        { internals
            | msgHistory = updatedMsgHistory
            , eventHistory = newEventList ++ internals.eventHistory
        }
    , newEventList
    )


detectClickEvents : List Msg -> List Event -> List Event
detectClickEvents msgHistory _ =
    let
        toMouseDown : Msg -> Maybe ( Time.Posix, Object )
        toMouseDown msg =
            case msg of
                MouseDown time object ->
                    Just ( time, object )

                _ ->
                    Nothing
    in
    case msgHistory of
        (MouseUp time releasedObj) :: recentMsgs ->
            case List.Extra.findMap toMouseDown recentMsgs of
                Just ( _, pressedObj ) ->
                    if releasedObj.id == pressedObj.id then
                        [ Click time releasedObj ]

                    else
                        []

                Nothing ->
                    []

        _ ->
            []


detectDoubleClickEvents : Int -> List Msg -> List Event -> List Event
detectDoubleClickEvents timeThreshold msgHistory eventHistory =
    case eventHistory of
        (Click newTime newObj) :: (Click oldTime oldObj) :: _ ->
            let
                timeDiffInMillis : Int
                timeDiffInMillis =
                    Time.posixToMillis newTime - Time.posixToMillis oldTime
            in
            case msgHistory of
                (MouseUp _ _) :: _ ->
                    if
                        (newObj.id == oldObj.id)
                            && (timeDiffInMillis < timeThreshold)
                    then
                        [ DoubleClick newTime newObj ]

                    else
                        []

                _ ->
                    []

        _ ->
            []


detectDragStartEvents : List Msg -> List Event -> List Event
detectDragStartEvents msgHistory _ =
    case msgHistory of
        (MouseMoved time _) :: (MouseDown _ obj) :: _ ->
            [ DragStart time obj ]

        _ ->
            []


detectDragEndEvents : List Msg -> List Event -> List Event
detectDragEndEvents msgHistory _ =
    case msgHistory of
        (MouseUp _ obj) :: (MouseMoved time _) :: _ ->
            [ DragEnd time obj ]

        _ ->
            []


eventsForDesktop : Screen -> Time.Posix -> (Msg -> msg) -> List (Attribute msg)
eventsForDesktop screen time toMsg =
    [ onMouseMove screen (toMsg << MouseMoved time)
    , onMouseDownForDesktop screen (toMsg << MouseDown time)
    , onMouseUpForDesktop screen (toMsg << MouseUp time)
    ]


onMouseDownForDesktop : Screen -> (Object -> msg) -> Attribute msg
onMouseDownForDesktop screen toMsg =
    Events.on "pointerdown"
        (Decode.map2
            (\cx cy ->
                let
                    position =
                        Coordinate.new ( cx, cy )
                            |> Screen.toScreenCoordinates screen
                in
                toMsg
                    { id = "desktop"
                    , offsetFromObjectOrigin = position
                    , mousePosition = position
                    }
            )
            (Decode.field "clientX" ViewHelpers.roundFloat)
            (Decode.field "clientY" ViewHelpers.roundFloat)
        )


onMouseUpForDesktop : Screen -> (Object -> msg) -> Attribute msg
onMouseUpForDesktop screen toMsg =
    Events.on "pointerup"
        (Decode.map2
            (\cx cy ->
                let
                    position =
                        Coordinate.new ( cx, cy )
                            |> Screen.toScreenCoordinates screen
                in
                toMsg
                    { id = "desktop"
                    , offsetFromObjectOrigin = position
                    , mousePosition = position
                    }
            )
            (Decode.field "clientX" ViewHelpers.roundFloat)
            (Decode.field "clientY" ViewHelpers.roundFloat)
        )


onMouseMove : Screen -> (Coordinate -> msg) -> Attribute msg
onMouseMove screen toMsg =
    Events.on "pointermove"
        (Decode.map2
            (\cx cy ->
                Coordinate.new ( cx, cy )
                    |> Screen.toScreenCoordinates screen
                    |> toMsg
            )
            (Decode.field "clientX" ViewHelpers.roundFloat)
            (Decode.field "clientY" ViewHelpers.roundFloat)
        )


onMouseDownForObject : String -> Coordinate -> Screen -> Time.Posix -> (Msg -> msg) -> Attribute msg
onMouseDownForObject objectId objectOrigin screen time toMsg =
    Events.stopPropagationOn "pointerdown"
        (Decode.map2
            (\cx cy ->
                let
                    mousePos =
                        Coordinate.new ( cx, cy )
                            |> Screen.toScreenCoordinates screen
                in
                ( toMsg <|
                    MouseDown time
                        { id = objectId
                        , offsetFromObjectOrigin =
                            Coordinate.minus objectOrigin mousePos
                        , mousePosition = mousePos
                        }
                , True
                )
            )
            (Decode.field "clientX" ViewHelpers.roundFloat)
            (Decode.field "clientY" ViewHelpers.roundFloat)
        )


onMouseUpForObject : String -> Coordinate -> Screen -> Time.Posix -> (Msg -> msg) -> Attribute msg
onMouseUpForObject objectId objectOrigin screen time toMsg =
    Events.stopPropagationOn "pointerup"
        (Decode.map2
            (\cx cy ->
                let
                    mousePos =
                        Coordinate.new ( cx, cy )
                            |> Screen.toScreenCoordinates screen
                in
                ( toMsg <|
                    MouseUp time
                        { id = objectId
                        , offsetFromObjectOrigin =
                            Coordinate.minus objectOrigin mousePos
                        , mousePosition = mousePos
                        }
                , True
                )
            )
            (Decode.field "clientX" ViewHelpers.roundFloat)
            (Decode.field "clientY" ViewHelpers.roundFloat)
        )
