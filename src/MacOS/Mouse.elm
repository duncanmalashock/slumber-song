module MacOS.Mouse exposing (Mouse, Msg, MsgData, buttonPressed, debug, eventsForDesktop, new, position, toMsg, update, x, y)

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
    , buttonPressed : Bool
    , msgHistory : List Msg
    , doubleClickTimingThreshold : Int
    }


position : Mouse -> Coordinate
position (Mouse internals) =
    internals.position


buttonPressed : Mouse -> Bool
buttonPressed (Mouse internals) =
    internals.buttonPressed


x : Mouse -> Int
x (Mouse internals) =
    Coordinate.x internals.position


y : Mouse -> Int
y (Mouse internals) =
    Coordinate.y internals.position


debug : Mouse -> String
debug (Mouse internals) =
    let
        msgToString : Msg -> String
        msgToString event =
            case event of
                NewMouseData args ->
                    "[ " ++ String.join ", " args.overObjIds ++ " ]"
    in
    internals.msgHistory
        |> List.take 10
        |> List.map msgToString
        |> String.join ", "


new : Mouse
new =
    Mouse
        { position = Coordinate.new ( 0, 0 )
        , buttonPressed = False
        , msgHistory = []
        , doubleClickTimingThreshold = 500
        }


type Msg
    = NewMouseData MsgData


type alias MsgData =
    { atTime : Time.Posix
    , buttonPressed : Bool
    , position : Coordinate
    , overObjIds : List String
    }


toMsg : MsgData -> Msg
toMsg data =
    NewMouseData data


maxMsgListLength : Int
maxMsgListLength =
    32


update : Msg -> Mouse -> Mouse
update msg (Mouse internals) =
    let
        updatedMsgHistory : List Msg
        updatedMsgHistory =
            (msg :: internals.msgHistory)
                |> List.take maxMsgListLength

        ( newPosition, newButtonPressed ) =
            case msg of
                NewMouseData args ->
                    ( args.position, args.buttonPressed )
    in
    Mouse
        { internals
            | msgHistory = updatedMsgHistory
            , position = newPosition
            , buttonPressed = newButtonPressed
        }


eventsForDesktop : ({ clientPos : ( Int, Int ), buttonPressed : Bool } -> msg) -> List (Attribute msg)
eventsForDesktop toMsg_ =
    [ onMouseMove toMsg_
    , onMouseDown toMsg_
    ]


onMouseMove : ({ clientPos : ( Int, Int ), buttonPressed : Bool } -> msg) -> Attribute msg
onMouseMove toMsg_ =
    Events.on "pointermove"
        (Decode.map3
            (\cx cy b ->
                toMsg_
                    { clientPos = ( cx, cy )
                    , buttonPressed = b == 1
                    }
            )
            (Decode.field "clientX" ViewHelpers.roundFloat)
            (Decode.field "clientY" ViewHelpers.roundFloat)
            (Decode.field "buttons" Decode.int)
        )


onMouseDown : ({ clientPos : ( Int, Int ), buttonPressed : Bool } -> msg) -> Attribute msg
onMouseDown toMsg_ =
    Events.on "pointerdown"
        (Decode.map3
            (\cx cy b ->
                toMsg_
                    { clientPos = ( cx, cy )
                    , buttonPressed = b == 1
                    }
            )
            (Decode.field "clientX" ViewHelpers.roundFloat)
            (Decode.field "clientY" ViewHelpers.roundFloat)
            (Decode.field "buttons" Decode.int)
        )


onMouseUp : ({ clientPos : ( Int, Int ), buttonPressed : Bool } -> msg) -> Attribute msg
onMouseUp toMsg_ =
    Events.on "pointerup"
        (Decode.map3
            (\cx cy b ->
                toMsg_
                    { clientPos = ( cx, cy )
                    , buttonPressed = b == 1
                    }
            )
            (Decode.field "clientX" ViewHelpers.roundFloat)
            (Decode.field "clientY" ViewHelpers.roundFloat)
            (Decode.field "buttons" Decode.int)
        )
