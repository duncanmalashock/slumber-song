module MacOS.Mouse exposing
    ( Event(..)
    , Mouse
    , Msg
    , MsgData
    , buttonPressed
    , debug
    , listeners
    , new
    , position
    , toMsg
    , update
    , x
    , y
    )

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
    , eventHistory : List Event
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
        , eventHistory = []
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
    16


maxEventListLength : Int
maxEventListLength =
    4


update : Msg -> Mouse -> ( Mouse, List Event )
update ((NewMouseData msgArgs) as msg) (Mouse internals) =
    let
        updatedMsgHistory : List Msg
        updatedMsgHistory =
            (msg :: internals.msgHistory)
                |> List.take maxMsgListLength

        ( newPosition, newButtonPressed ) =
            ( msgArgs.position, msgArgs.buttonPressed )

        buttonChanged : Bool
        buttonChanged =
            msgArgs.buttonPressed /= internals.buttonPressed

        detectedEvents : List Event
        detectedEvents =
            detectEvents
                ( buttonChanged, newButtonPressed )
                updatedMsgHistory
                internals.eventHistory
                msgArgs.overObjIds
    in
    ( Mouse
        { internals
            | msgHistory = updatedMsgHistory
            , position = newPosition
            , buttonPressed = newButtonPressed
            , eventHistory =
                detectedEvents
                    ++ internals.eventHistory
                    |> List.take maxEventListLength
        }
    , detectedEvents
    )


type Event
    = Clicked String
    | DoubleClicked String
    | DragStarted String
    | MouseReleased


detectEvents : ( Bool, Bool ) -> List msg -> List Event -> List String -> List Event
detectEvents ( buttonChanged, buttonIsDown ) recentMsgs recentEvents overObjIds =
    if buttonChanged && buttonIsDown then
        List.map DragStarted overObjIds

    else if buttonChanged && not buttonIsDown then
        [ MouseReleased ]

    else
        []


listeners : ({ clientPos : ( Int, Int ), buttonPressed : Bool } -> msg) -> List (Attribute msg)
listeners toMsg_ =
    [ Events.on "pointermove" (mouseEventDecoder toMsg_)
    , Events.on "pointerdown" (mouseEventDecoder toMsg_)
    , Events.on "pointerup" (mouseEventDecoder toMsg_)
    ]


mouseEventDecoder : ({ clientPos : ( Int, Int ), buttonPressed : Bool } -> msg) -> Decoder msg
mouseEventDecoder toMsg_ =
    Decode.map3
        (\cx cy b ->
            toMsg_
                { clientPos = ( cx, cy )
                , buttonPressed = b == 1
                }
        )
        (Decode.field "clientX" ViewHelpers.roundFloat)
        (Decode.field "clientY" ViewHelpers.roundFloat)
        (Decode.field "buttons" Decode.int)
