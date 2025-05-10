module Main exposing (main)

import Apps.WindSleepers as WindSleepers
import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Instruction as Instruction exposing (Instruction)
import MacOS.MenuBar as MenuBar exposing (MenuBar)
import MacOS.Mouse as Mouse exposing (Mouse)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.Screen as Screen exposing (Screen)
import MacOS.UI as UI exposing (UI)
import MacOS.UI.FillPattern as FillPattern
import MacOS.UI.Helpers as UIHelpers exposing (domIds, imgURL, px)
import MacOS.UI.Object as UIObject exposing (Object)
import MacOS.UI.View as View exposing (View)
import MacOS.UI.View.Rect
import MacOS.UI.View.Window as Window
import Set
import Task
import Time


viewDebugger : Model -> Html Msg
viewDebugger model =
    div
        [ id domIds.debugger
        , style "position" "absolute"
        , style "bottom" (px 16)
        , style "left" (px 16)
        , style "width" (px (Screen.width model.screen - 32))
        ]
        [ div
            [ style "background-color" "black"
            , style "color" "white"
            , style "font-family" "Geneva"
            , style "padding" "0 6px"
            ]
            [ div [] [ text "" ]
            ]
        ]


type alias Model =
    { currentTime : Time.Posix
    , screen : Screen
    , menuBar : MenuBar
    , mouse : Mouse
    , ui : UI Msg
    , dragging : Maybe Dragging
    , instructions : List (Instruction Msg)
    , currentInstruction : Maybe { timeStarted : Time.Posix, instruction : Instruction Msg }
    }


type alias Dragging =
    { objId : String
    , rect : Rect
    , offset : Coordinate
    , view : View Msg
    }


type alias Flags =
    { browserDimensions : { x : Int, y : Int }
    , devicePixelRatio : Float
    , currentTimeInMS : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { currentTime = Time.millisToPosix flags.currentTimeInMS
      , screen =
            Screen.new
                { screenInPixels = ( 512, 342 )
                , browser = flags.browserDimensions
                , devicePixelRatio = flags.devicePixelRatio
                }
      , menuBar = MenuBar.new []
      , mouse = Mouse.new
      , ui =
            UI.new
                |> UI.createObject
                    (UIObject.new
                        { id = domIds.desktop
                        , rect = Rect.new ( 0, 0 ) ( 512, 342 )
                        }
                    )
                |> UI.attachObject
                    { objectId = domIds.desktop
                    , parentId = domIds.root
                    }
                |> UI.createObject
                    (UIObject.new
                        { id = domIds.desktopRectangles
                        , rect = Rect.new ( 0, 0 ) ( 512, 342 )
                        }
                    )
                |> UI.attachObject
                    { objectId = domIds.desktopRectangles
                    , parentId = domIds.desktop
                    }
                |> UI.createObject
                    (UIObject.new
                        { id = "zoomRect0"
                        , rect = Rect.new ( 0, 0 ) ( 0, 0 )
                        }
                        |> UIObject.setView
                            (View.rect MacOS.UI.View.Rect.StyleDotted)
                    )
                |> UI.attachObject
                    { objectId = "zoomRect0"
                    , parentId = domIds.desktopRectangles
                    }
                |> UI.createObject
                    (UIObject.new
                        { id = "zoomRect1"
                        , rect = Rect.new ( 0, 0 ) ( 0, 0 )
                        }
                        |> UIObject.setView
                            (View.rect MacOS.UI.View.Rect.StyleDotted)
                    )
                |> UI.attachObject
                    { objectId = "zoomRect1"
                    , parentId = domIds.desktopRectangles
                    }
                |> UI.createObject
                    (UIObject.new
                        { id = "zoomRect2"
                        , rect = Rect.new ( 0, 0 ) ( 0, 0 )
                        }
                        |> UIObject.setView
                            (View.rect MacOS.UI.View.Rect.StyleDotted)
                    )
                |> UI.attachObject
                    { objectId = "zoomRect2"
                    , parentId = domIds.desktopRectangles
                    }
                |> UI.createObject
                    (UIObject.new
                        { id = "zoomRect3"
                        , rect = Rect.new ( 0, 0 ) ( 0, 0 )
                        }
                        |> UIObject.setView
                            (View.rect MacOS.UI.View.Rect.StyleDotted)
                    )
                |> UI.attachObject
                    { objectId = "zoomRect3"
                    , parentId = domIds.desktopRectangles
                    }
                |> UI.createObject
                    (UIObject.new
                        { id = "windows"
                        , rect = Rect.new ( 0, 0 ) ( 512, 342 )
                        }
                    )
                |> UI.attachObject
                    { objectId = "windows"
                    , parentId = domIds.desktop
                    }
      , dragging = Nothing
      , instructions = WindSleepers.program
      , currentInstruction = Nothing
      }
    , Cmd.none
    )


type Msg
    = Tick Time.Posix
    | BrowserResized Int Int
    | MouseUpdated { clientPos : ( Int, Int ), buttonPressed : Bool }
    | MouseEvent Mouse.Event
    | ClickedCloseBoxForWindow String


handleInstruction : { timeStarted : Time.Posix, instruction : Instruction Msg } -> Model -> ( Model, Cmd Msg )
handleInstruction { timeStarted, instruction } model =
    case instruction of
        Instruction.AnimateZoom { from, to, zoomingIn } ->
            let
                animationDuration : number
                animationDuration =
                    250

                animationPhase : Int
                animationPhase =
                    (toFloat (Time.posixToMillis model.currentTime - Time.posixToMillis timeStarted)
                        / animationDuration
                    )
                        * 16
                        |> ceiling

                animationComplete : Bool
                animationComplete =
                    Time.posixToMillis model.currentTime - Time.posixToMillis timeStarted >= animationDuration

                zoomRect : Int -> Rect
                zoomRect x =
                    -- x ranges from 0 to 11
                    if zoomingIn then
                        let
                            flopped =
                                0.69 ^ toFloat ((x * -1 + 11) + 1)
                        in
                        Rect.interpolate from to flopped

                    else
                        Rect.interpolate to from (0.69 ^ toFloat (x + 1))

                zoomRects : List ( String, Rect )
                zoomRects =
                    if animationComplete then
                        List.range 0 3
                            |> List.map
                                (\i ->
                                    ( "zoomRect" ++ String.fromInt i
                                    , Rect.new ( 0, 0 ) ( 0, 0 )
                                    )
                                )

                    else
                        List.range 0 3
                            |> List.map
                                (\i ->
                                    let
                                        clamped : Int
                                        clamped =
                                            (i + (animationPhase - 4))
                                                |> Basics.clamp 0 11
                                    in
                                    ( "zoomRect" ++ String.fromInt i
                                    , zoomRect clamped
                                    )
                                )

                updatedUI : UI Msg
                updatedUI =
                    zoomRects
                        |> List.map (\( key, rect ) -> ( key, UIObject.setRect rect ))
                        |> (\updaters -> UI.updateList updaters model.ui)

                updatedCurrentInstruction : Maybe { timeStarted : Time.Posix, instruction : Instruction Msg }
                updatedCurrentInstruction =
                    if animationComplete then
                        Nothing

                    else
                        model.currentInstruction
            in
            ( { model
                | currentInstruction = updatedCurrentInstruction
                , ui = updatedUI
              }
            , Cmd.none
            )

        Instruction.RemoveWindow { withId } ->
            let
                updatedUI : UI.UI Msg
                updatedUI =
                    model.ui
                        |> UI.remove withId
            in
            ( { model
                | currentInstruction = Nothing
                , ui = updatedUI
              }
            , Cmd.none
            )

        Instruction.CreateWindow { withId, window } ->
            let
                updatedUI : UI.UI Msg
                updatedUI =
                    model.ui
                        |> UI.createObject
                            (UIObject.new
                                { id = withId
                                , rect = window.rect
                                }
                                |> UIObject.setView
                                    (View.window window)
                                |> UIObject.setDragOptions
                                    { traveling = View.rect MacOS.UI.View.Rect.StyleDotted
                                    }
                            )
                        |> UI.attachObject
                            { objectId = withId
                            , parentId = "windows"
                            }
            in
            ( { model
                | currentInstruction = Nothing
                , ui = updatedUI
              }
            , Cmd.none
            )

        Instruction.CreateObject { object } ->
            let
                updatedUI : UI.UI Msg
                updatedUI =
                    model.ui
                        |> UI.createObject object
            in
            ( { model
                | currentInstruction = Nothing
                , ui = updatedUI
              }
            , Cmd.none
            )

        Instruction.AttachObject { objectId, parentId } ->
            let
                updatedUI : UI.UI Msg
                updatedUI =
                    model.ui
                        |> UI.attachObject
                            { objectId = objectId
                            , parentId = parentId
                            }
            in
            ( { model
                | currentInstruction = Nothing
                , ui = updatedUI
              }
            , Cmd.none
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            case model.currentInstruction of
                Nothing ->
                    case model.instructions of
                        [] ->
                            ( { model
                                | currentTime = time
                                , mouse =
                                    model.mouse
                                        |> Mouse.unlock
                                        |> Mouse.setCursorPointer
                              }
                            , Cmd.none
                            )

                        first :: rest ->
                            ( { model
                                | currentTime = time
                                , mouse =
                                    model.mouse
                                        |> Mouse.lock
                                        |> Mouse.setCursorWatch
                                , instructions = rest
                                , currentInstruction =
                                    Just
                                        { timeStarted = time
                                        , instruction = first
                                        }
                              }
                            , Cmd.none
                            )

                Just { timeStarted, instruction } ->
                    { model | currentTime = time }
                        |> handleInstruction
                            { timeStarted = timeStarted
                            , instruction = instruction
                            }

        BrowserResized newWidth newHeight ->
            ( { model
                | screen =
                    model.screen
                        |> Screen.update
                            { x = newWidth
                            , y = newHeight
                            }
              }
            , Cmd.none
            )

        MouseUpdated args ->
            let
                newMousePos : Coordinate
                newMousePos =
                    Coordinate.new args.clientPos
                        |> Screen.toScreenCoordinates model.screen

                newMouseButtonState : Bool
                newMouseButtonState =
                    args.buttonPressed

                hitTestResults : List String
                hitTestResults =
                    UI.hitTest newMousePos model.ui

                domUpdate : Mouse.DomUpdate
                domUpdate =
                    { atTime = model.currentTime
                    , buttonPressed = args.buttonPressed
                    , position = newMousePos
                    , overObjIds = hitTestResults
                    }

                mouseStateChanged : Bool
                mouseStateChanged =
                    (newMousePos /= Mouse.position model.mouse)
                        || (newMouseButtonState /= Mouse.buttonPressed model.mouse)

                ( updatedMouse, newMouseEvents ) =
                    if mouseStateChanged then
                        Mouse.update
                            (Mouse.toMsg domUpdate)
                            model.mouse

                    else
                        ( model.mouse, [] )

                pickedId : Maybe String
                pickedId =
                    UI.topmostObjectInList hitTestResults model.ui

                eventCmds : Cmd Msg
                eventCmds =
                    if Mouse.interactionsAllowed model.mouse then
                        newMouseEvents
                            |> Mouse.filterMouseEventsByObjectId pickedId
                            |> List.map MouseEvent
                            |> List.map sendMsg
                            |> Cmd.batch

                    else
                        Cmd.none

                updatedDragging : Maybe Dragging
                updatedDragging =
                    case model.dragging of
                        Just dragging ->
                            Just
                                { dragging
                                    | rect =
                                        Rect.setPosition
                                            (Coordinate.plus newMousePos dragging.offset)
                                            dragging.rect
                                }

                        Nothing ->
                            Nothing
            in
            ( { model
                | mouse = updatedMouse
                , dragging = updatedDragging
              }
            , eventCmds
            )

        ClickedCloseBoxForWindow windowId ->
            ( { model
                | dragging = Nothing
                , instructions =
                    model.instructions
                        ++ [ Instruction.RemoveWindow { withId = windowId }
                           , Instruction.AnimateZoom
                                { from = Rect.new ( 64, 64 ) ( 200, 200 )
                                , to = Rect.new ( 450, 40 ) ( 32, 32 )
                                , zoomingIn = False
                                }
                           ]
              }
            , Cmd.none
            )

        MouseEvent event ->
            let
                maybeMsgFromEventHandler : Maybe Msg
                maybeMsgFromEventHandler =
                    UI.mouseEventToHandlerMsg event model.ui

                cmd : Cmd Msg
                cmd =
                    Maybe.map sendMsg maybeMsgFromEventHandler
                        |> Maybe.withDefault Cmd.none
            in
            case event of
                Mouse.MouseDown objId ->
                    let
                        updatedModel : Model
                        updatedModel =
                            { model
                                | ui =
                                    UI.update objId
                                        (UIObject.setSelected True)
                                        model.ui
                            }
                    in
                    ( updatedModel
                    , cmd
                    )

                Mouse.MouseUp ->
                    let
                        updatedUI : UI.UI Msg
                        updatedUI =
                            case model.dragging of
                                Just dragging ->
                                    UI.update dragging.objId
                                        (UIObject.setPosition (Rect.position dragging.rect))
                                        model.ui

                                Nothing ->
                                    model.ui
                    in
                    ( { model
                        | dragging = Nothing
                        , ui = updatedUI
                      }
                    , cmd
                    )

                Mouse.Click objId ->
                    ( model
                    , cmd
                    )

                Mouse.DoubleClick objId ->
                    ( model
                    , cmd
                    )

                Mouse.DragStart objId ->
                    let
                        maybeDraggedObject : Maybe (Object Msg)
                        maybeDraggedObject =
                            UI.getObject model.ui objId

                        maybeDragView : Maybe (View Msg)
                        maybeDragView =
                            Maybe.andThen UIObject.getDragOptions maybeDraggedObject
                                |> Maybe.map .traveling

                        updatedModel : Model
                        updatedModel =
                            case ( maybeDraggedObject, maybeDragView ) of
                                ( Just obj, Just dragView ) ->
                                    let
                                        mouseOffset : Coordinate
                                        mouseOffset =
                                            Coordinate.minus
                                                (Mouse.position model.mouse)
                                                (UIObject.position obj)
                                    in
                                    { model
                                        | dragging =
                                            Just
                                                { objId = objId
                                                , rect = UIObject.rect obj
                                                , offset = mouseOffset
                                                , view = dragView
                                                }
                                        , ui = UI.bringObjectToFront objId model.ui
                                    }

                                _ ->
                                    model
                    in
                    ( updatedModel
                    , cmd
                    )


sendMsg : Msg -> Cmd Msg
sendMsg msg =
    Task.perform identity (Task.succeed msg)


view : Model -> Html Msg
view model =
    {-
       UI Layers
       (From bottom to top)

       - Desktop
       - Desktop Objects
       - Desktop-related Rectangles
       - Windows
       - Window-related Rectangles
       - Menu Bar & Menus
       - Dialogs
       - Rounded Screen Corners
       - Debugger
       - Cursor
    -}
    div
        ([ style "width" (px (Screen.width model.screen))
         , style "height" (px (Screen.height model.screen))
         , style "background-color" "black"
         , style "background-image" FillPattern.dither50
         , style "position" "relative"
         , style "overflow" "hidden"
         , style "cursor" "none"
         ]
            ++ Mouse.listeners MouseUpdated
            ++ Screen.scaleAttrs model.screen
        )
        [ UI.view model.ui
        , viewDraggedObject model
        , MenuBar.view (Screen.width model.screen) model.menuBar
        , viewScreenCorners (Screen.logical model.screen)
        , viewDebugger model
        , Mouse.view model.mouse
        ]


viewDraggedObject : Model -> Html Msg
viewDraggedObject model =
    case model.dragging of
        Just dragging ->
            View.view dragging.rect dragging.view []

        Nothing ->
            UIHelpers.none


type Corner
    = TopLeftCorner
    | TopRightCorner
    | BottomLeftCorner
    | BottomRightCorner


viewScreenCorners : Rect -> Html msg
viewScreenCorners screen =
    let
        cornerSize : Int
        cornerSize =
            5

        attrs : Corner -> List (Html.Attribute msg)
        attrs corner =
            case corner of
                TopLeftCorner ->
                    [ style "background-image" (imgURL "MacOS/corner-tl.gif")
                    , style "top" (px 0)
                    , style "left" (px 0)
                    ]

                TopRightCorner ->
                    [ style "background-image" (imgURL "MacOS/corner-tr.gif")
                    , style "top" (px 0)
                    , style "right" (px 0)
                    ]

                BottomLeftCorner ->
                    [ style "background-image" (imgURL "MacOS/corner-bl.gif")
                    , style "bottom" (px 0)
                    , style "left" (px 0)
                    ]

                BottomRightCorner ->
                    [ style "background-image" (imgURL "MacOS/corner-br.gif")
                    , style "bottom" (px 0)
                    , style "right" (px 0)
                    ]

        viewCorner : Corner -> Html msg
        viewCorner c =
            div
                ([ style "width" (px cornerSize)
                 , style "height" (px cornerSize)
                 , style "position" "absolute"
                 ]
                    ++ attrs c
                )
                []

        corners : List Corner
        corners =
            [ TopLeftCorner
            , TopRightCorner
            , BottomLeftCorner
            , BottomRightCorner
            ]
    in
    div
        [ id domIds.screenCorners
        , style "width" (px (Rect.width screen))
        , style "height" (px (Rect.height screen))
        , style "position" "absolute"
        , style "pointer-events" "none"
        ]
        (List.map viewCorner corners)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize BrowserResized
        , Time.every 10 Tick
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
