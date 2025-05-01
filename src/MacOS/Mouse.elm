module MacOS.Mouse exposing (Mouse, Msg(..), eventsForBaseElement, new, update)

import Html exposing (Attribute)
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Rect exposing (Rect)
import MacOS.Screen as Screen exposing (Screen)
import MacOS.ViewHelpers as ViewHelpers


type Mouse
    = Mouse Internals


type Button
    = ButtonUp
    | ButtonDown


type alias Internals =
    { button : Button
    , position : Coordinate
    , lastObjectInteraction : Maybe ObjectInteraction
    }


type ObjectInteraction
    = PressedMouseOver Object
    | ReleasedMouseOver Object


new : Mouse
new =
    Mouse
        { button = ButtonUp
        , position = Coordinate.new ( 0, 0 )
        , lastObjectInteraction = Nothing
        }


type Msg
    = MouseMoved Coordinate
    | MouseUp Object
    | MouseDown Object


type alias Object =
    { id : String
    , offsetFromObjectOrigin : Coordinate
    , mousePosition : Coordinate
    }


update : Msg -> Mouse -> Mouse
update msg (Mouse internals) =
    case msg of
        MouseMoved newPosition ->
            Mouse
                { internals
                    | position = newPosition
                }

        MouseDown object ->
            Mouse
                { internals
                    | button = ButtonDown
                    , lastObjectInteraction =
                        Just (PressedMouseOver object)
                }

        MouseUp object ->
            Mouse
                { internals
                    | button = ButtonUp
                    , lastObjectInteraction =
                        Just (ReleasedMouseOver object)
                }


eventsForBaseElement : Screen -> (Msg -> msg) -> List (Attribute msg)
eventsForBaseElement screen toMsg =
    [ onMouseMove screen (toMsg << MouseMoved)
    , onMouseDown screen (toMsg << MouseDown)
    , onMouseUp screen (toMsg << MouseUp)
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
