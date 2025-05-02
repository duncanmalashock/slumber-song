module MacOS.Mouse exposing (Mouse, Msg(..), debugEvents, eventsForDesktop, new, onMouseDownForObject, onMouseUpForObject, update)

import Html exposing (Attribute)
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
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
    = ClickedObject Time.Posix Object
    | DraggedObject Time.Posix Object
    | DoubleClickedObject Time.Posix Object


debugEvents : Mouse -> String
debugEvents (Mouse internals) =
    internals.eventHistory
        |> List.map eventToString
        |> String.join ", "


eventToString : Event -> String
eventToString event =
    case event of
        ClickedObject _ { id } ->
            "ClickedObject " ++ id

        DraggedObject _ { id } ->
            "DraggedObject " ++ id

        DoubleClickedObject _ { id } ->
            "DoubleClickedObject " ++ id


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
    4


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
        foldEvents detectorFn events =
            detectorFn updatedMsgHistory events ++ events

        newEventList : List Event
        newEventList =
            List.foldl foldEvents
                internals.eventHistory
                [ detectClickEvents
                , detectDragEvents
                , detectDoubleClickEvents internals.doubleClickTimingThreshold
                ]
    in
    ( Mouse
        { internals
            | msgHistory = updatedMsgHistory
            , eventHistory = newEventList
        }
    , newEventList
    )


detectClickEvents : List Msg -> List Event -> List Event
detectClickEvents msgHistory _ =
    case msgHistory of
        (MouseUp time releasedObj) :: (MouseDown _ pressedObj) :: _ ->
            if releasedObj.id == pressedObj.id then
                [ ClickedObject time releasedObj ]

            else
                []

        _ ->
            []


detectDoubleClickEvents : Int -> List Msg -> List Event -> List Event
detectDoubleClickEvents timeThreshold msgHistory eventHistory =
    case eventHistory of
        (ClickedObject newTime newObj) :: (ClickedObject oldTime oldObj) :: _ ->
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
                        [ DoubleClickedObject newTime newObj ]

                    else
                        []

                _ ->
                    []

        _ ->
            []


detectDragEvents : List Msg -> List Event -> List Event
detectDragEvents msgHistory _ =
    case msgHistory of
        (MouseMoved time _) :: (MouseDown _ pressedObj) :: _ ->
            [ DraggedObject time pressedObj ]

        _ ->
            []


eventsForDesktop : Screen -> Time.Posix -> (Msg -> msg) -> List (Attribute msg)
eventsForDesktop screen time toMsg =
    [ onMouseMove screen (toMsg << MouseMoved time)
    , onMouseDown screen (toMsg << MouseDown time)
    , onMouseUp screen (toMsg << MouseUp time)
    ]


onMouseDown : Screen -> (Object -> msg) -> Attribute msg
onMouseDown screen toMsg =
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


onMouseUp : Screen -> (Object -> msg) -> Attribute msg
onMouseUp screen toMsg =
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
