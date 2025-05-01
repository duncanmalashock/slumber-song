module MacOS.Mouse exposing (Mouse, Msg(..), eventsForBaseElement, new, update)

import Html exposing (Attribute)
import Html.Events as Events exposing (..)
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
    }


new : Mouse
new =
    Mouse
        { button = ButtonUp
        , position = Coordinate.new ( 0, 0 )
        }


type Msg
    = MouseMoved Coordinate
    | MouseUp Object
    | MouseDown Object


type Object
    = Object
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

        MouseUp object ->
            Mouse
                { internals
                    | button = ButtonUp
                }

        MouseDown object ->
            Mouse
                { internals
                    | button = ButtonUp
                }


eventsForBaseElement : Screen -> (Msg -> msg) -> List (Attribute msg)
eventsForBaseElement screen toMsg =
    [ onPointerMove screen (toMsg << MouseMoved) ]


onPointerMove : Screen -> (Coordinate -> msg) -> Attribute msg
onPointerMove screen toMsg =
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


onPointerUp : msg -> Attribute msg
onPointerUp msg =
    Events.on "pointerup" (Decode.succeed msg)
