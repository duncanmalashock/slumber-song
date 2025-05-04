module MacOS.Mouse exposing (Mouse, Msg, MsgData, debug, eventsForDesktop, new, position, toMsg, update, x, y)

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
    , doubleClickTimingThreshold : Int
    }


position : Mouse -> Coordinate
position (Mouse internals) =
    internals.position


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
        |> List.take 1
        |> List.map msgToString
        |> String.join ", "


new : Mouse
new =
    Mouse
        { position = Coordinate.new ( 0, 0 )
        , msgHistory = []
        , doubleClickTimingThreshold = 500
        }


type Msg
    = NewMouseData MsgData


type alias MsgData =
    { atTime : Time.Posix
    , mouseButtonPressed : Bool
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

        newPosition : Coordinate
        newPosition =
            case msg of
                NewMouseData args ->
                    args.position
    in
    Mouse
        { internals
            | msgHistory = updatedMsgHistory
            , position = newPosition
        }


eventsForDesktop : ({ clientPos : ( Int, Int ), buttonClicked : Bool } -> msg) -> List (Attribute msg)
eventsForDesktop toMsg_ =
    [ onMouseMove toMsg_
    , onMouseDown toMsg_
    ]


onMouseMove : ({ clientPos : ( Int, Int ), buttonClicked : Bool } -> msg) -> Attribute msg
onMouseMove toMsg_ =
    Events.on "pointermove"
        (Decode.map3
            (\cx cy b ->
                toMsg_
                    { clientPos = ( cx, cy )
                    , buttonClicked = b == 1
                    }
            )
            (Decode.field "clientX" ViewHelpers.roundFloat)
            (Decode.field "clientY" ViewHelpers.roundFloat)
            (Decode.field "buttons" Decode.int)
        )


onMouseDown : ({ clientPos : ( Int, Int ), buttonClicked : Bool } -> msg) -> Attribute msg
onMouseDown toMsg_ =
    Events.on "pointerdown"
        (Decode.map3
            (\cx cy b ->
                toMsg_
                    { clientPos = ( cx, cy )
                    , buttonClicked = b == 1
                    }
            )
            (Decode.field "clientX" ViewHelpers.roundFloat)
            (Decode.field "clientY" ViewHelpers.roundFloat)
            (Decode.field "buttons" Decode.int)
        )


onMouseUp : ({ clientPos : ( Int, Int ), buttonClicked : Bool } -> msg) -> Attribute msg
onMouseUp toMsg_ =
    Events.on "pointerup"
        (Decode.map3
            (\cx cy b ->
                toMsg_
                    { clientPos = ( cx, cy )
                    , buttonClicked = b == 1
                    }
            )
            (Decode.field "clientX" ViewHelpers.roundFloat)
            (Decode.field "clientY" ViewHelpers.roundFloat)
            (Decode.field "buttons" Decode.int)
        )
