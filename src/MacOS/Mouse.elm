module MacOS.Mouse exposing
    ( Mouse, new
    , position, x, y, buttonPressed, locked
    , buttonJustPressed, buttonJustReleased
    , update, Msg
    , DomUpdate, toMsg
    , lock, unlock
    , setCursorPointer, setCursorWatch
    , listeners
    , view
    )

{-| The mouse's position and button state. Can produce mouse events for user interaction.


# Mouse

@docs Mouse, new


# Query

@docs position, x, y, buttonPressed, locked
@docs buttonJustPressed, buttonJustReleased


# Update

@docs update, Msg
@docs DomUpdate, toMsg
@docs lock, unlock
@docs setCursorPointer, setCursorWatch


# Mouse interactions

@docs listeners


# View

@docs view

-}

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (id, style)
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Rect exposing (Rect)
import MacOS.Screen as Screen exposing (Screen)
import MacOS.UI.Helpers exposing (domIds, imgURL, px)
import Time


type Mouse
    = Mouse Internals


type alias Internals =
    { position : Coordinate
    , buttonPressed : Bool
    , cursor : Maybe Cursor
    , locked : Bool
    , msgHistory : List Msg
    }


position : Mouse -> Coordinate
position (Mouse internals) =
    internals.position


buttonPressed : Mouse -> Bool
buttonPressed (Mouse internals) =
    internals.buttonPressed


locked : Mouse -> Bool
locked (Mouse internals) =
    internals.locked


x : Mouse -> Int
x (Mouse internals) =
    Coordinate.x internals.position


y : Mouse -> Int
y (Mouse internals) =
    Coordinate.y internals.position


buttonJustPressed : Mouse -> Bool
buttonJustPressed (Mouse internals) =
    case internals.msgHistory of
        (NewMouseData last) :: (NewMouseData nextToLast) :: _ ->
            last.buttonPressed && not nextToLast.buttonPressed

        _ ->
            False


buttonJustReleased : Mouse -> Bool
buttonJustReleased (Mouse internals) =
    case internals.msgHistory of
        (NewMouseData last) :: (NewMouseData nextToLast) :: _ ->
            not last.buttonPressed && nextToLast.buttonPressed

        _ ->
            False


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


type Cursor
    = CursorPointer
    | CursorWatch


new : Mouse
new =
    Mouse
        { position = Coordinate.new ( 16, 16 )
        , buttonPressed = False
        , cursor = Just CursorPointer
        , locked = False
        , msgHistory = []
        }


type Msg
    = NewMouseData DomUpdate


type alias DomUpdate =
    { atTime : Time.Posix
    , buttonPressed : Bool
    , position : Coordinate
    , overObjIds : List String
    }


toMsg : DomUpdate -> Msg
toMsg data =
    NewMouseData data


maxMsgListLength : Int
maxMsgListLength =
    16


update : Msg -> Mouse -> Mouse
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
    in
    Mouse
        { internals
            | msgHistory = updatedMsgHistory
            , position = newPosition
            , buttonPressed = newButtonPressed
        }


lock : Mouse -> Mouse
lock (Mouse internals) =
    Mouse
        { internals
            | locked = True
        }


unlock : Mouse -> Mouse
unlock (Mouse internals) =
    Mouse
        { internals
            | locked = False
        }


setCursorPointer : Mouse -> Mouse
setCursorPointer (Mouse internals) =
    Mouse
        { internals
            | cursor = Just CursorPointer
        }


setCursorWatch : Mouse -> Mouse
setCursorWatch (Mouse internals) =
    Mouse
        { internals
            | cursor = Just CursorWatch
        }


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
        (Decode.field "clientX" positionDecoder)
        (Decode.field "clientY" positionDecoder)
        (Decode.field "buttons" Decode.int)


positionDecoder : Decoder Int
positionDecoder =
    Decode.float
        |> Decode.map floor


view : Mouse -> Html msg
view (Mouse internals) =
    let
        cursorData =
            case internals.cursor of
                Just CursorPointer ->
                    { image = "MacOS/cursor-pointer.gif"
                    , offsetX = -4
                    , offsetY = -1
                    }

                Just CursorWatch ->
                    { image = "MacOS/cursor-watch.gif"
                    , offsetX = -9
                    , offsetY = -9
                    }

                Nothing ->
                    { image = ""
                    , offsetX = 0
                    , offsetY = 0
                    }
    in
    div
        [ id domIds.mouse
        , style "position" "relative"
        , style "left" (px (x (Mouse internals) + cursorData.offsetX))
        , style "top" (px (y (Mouse internals) + cursorData.offsetY))
        , style "width" (px 16)
        , style "height" (px 16)
        , style "background-image" (imgURL cursorData.image)
        , style "pointer-events" "none"
        ]
        []
