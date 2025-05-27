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
import MacOS.MouseEvent as MouseEvent exposing (MouseEvent)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.Screen as Screen exposing (Screen)
import MacOS.ToAppMsg as ToAppMsg exposing (ToAppMsg)
import MacOS.UI as UI exposing (UI)
import MacOS.UI.FillPattern as FillPattern
import MacOS.UI.Helpers as UIHelpers exposing (domIds, imgURL, px)
import MacOS.UI.Object as UIObject exposing (Object)
import MacOS.UI.View as View exposing (View)
import MacOS.UI.View.Rectangle
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
            [--     div [] [ text ("buttonJustPressed: " ++ Debug.toString (Mouse.buttonJustPressed model.mouse)) ]
             -- , div [] [ text ("lastMouseDown: " ++ Debug.toString model.lastMouseDown) ]
             -- , div [] [ text ("lastMouseUp: " ++ Debug.toString model.lastMouseUp) ]
             -- , div [] [ text ("lastClick: " ++ Debug.toString model.lastClick) ]
             -- , div [] [ text ("lastDoubleClick: " ++ Debug.toString model.lastDoubleClick) ]
            ]
        ]


type alias Model =
    { currentTime : Time.Posix
    , screen : Screen
    , menuBar : MenuBar
    , mouse : Mouse
    , lastMouseDown : Maybe { objectId : String, time : Time.Posix, coordinate : Coordinate }
    , lastMouseDownObject : Maybe (Object Msg)
    , lastMouseUp : Maybe { objectId : String, time : Time.Posix }
    , lastClick : Maybe { objectId : String, time : Time.Posix }
    , lastDoubleClick : Maybe { objectId : String, time : Time.Posix }
    , maxTimeBetweenDoubleClicks : Int
    , ui : UI Msg
    , pickedObjectId : Maybe String
    , debug : Maybe String
    , dragging : Maybe Dragging
    , app : WindSleepers.Model
    , instructions : List (Instruction Msg)
    , currentInstruction : Maybe { timeStarted : Time.Posix, instruction : Instruction Msg }
    , zoomRects : List Rect
    }


type alias Dragging =
    { objectId : String
    , absolutePositionAtClick : Coordinate
    , dragDelta : Coordinate
    , clickOffset : Coordinate
    , drawRect : Rect
    , view : View Msg
    , originRect : Rect
    }


type alias Flags =
    { browserDimensions : { x : Int, y : Int }
    , devicePixelRatio : Float
    , currentTimeInMS : Int
    }


type alias ObjectId =
    String


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        screen : Screen
        screen =
            Screen.new
                { screenInPixels = ( 512, 342 )
                , browser = flags.browserDimensions
                , devicePixelRatio = flags.devicePixelRatio
                }

        ( app, appInitInstructions ) =
            WindSleepers.init
    in
    ( { currentTime = Time.millisToPosix flags.currentTimeInMS
      , screen = screen
      , menuBar = MenuBar.new []
      , mouse = Mouse.new
      , lastMouseDown = Nothing
      , lastMouseDownObject = Nothing
      , lastMouseUp = Nothing
      , lastClick = Nothing
      , lastDoubleClick = Nothing
      , maxTimeBetweenDoubleClicks = 500
      , ui =
            UI.new screen
                |> UI.createObject
                    (UIObject.new
                        { id = domIds.desktop
                        , rect = Screen.logical screen
                        }
                    )
                |> UI.attachObject
                    { objectId = domIds.desktop
                    , parentId = domIds.root
                    , rect = Screen.logical screen
                    }
                |> UI.createObject
                    (UIObject.new
                        { id = domIds.windows
                        , rect = Screen.logical screen
                        }
                    )
                |> UI.attachObject
                    { objectId = domIds.windows
                    , parentId = domIds.desktop
                    , rect = Screen.logical screen
                    }
      , pickedObjectId = Nothing
      , debug = Nothing
      , dragging = Nothing
      , app = app
      , instructions = appInitInstructions
      , currentInstruction = Nothing
      , zoomRects = []
      }
    , Cmd.none
    )


type Msg
    = Tick Time.Posix
    | BrowserResized Int Int
    | MouseUpdated { clientPos : ( Int, Int ), buttonPressed : Bool }
    | MouseEvent MouseEvent
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
                    if zoomingIn then
                        let
                            flopped : Float
                            flopped =
                                0.69 ^ toFloat ((x * -1 + 11) + 1)
                        in
                        Rect.interpolate from to flopped

                    else
                        Rect.interpolate to from (0.69 ^ toFloat (x + 1))

                zoomRects : List Rect
                zoomRects =
                    if animationComplete then
                        []

                    else
                        List.range 0 3
                            |> List.filterMap
                                (\i ->
                                    let
                                        interp : Int
                                        interp =
                                            i + (animationPhase - 4)
                                    in
                                    if interp >= 0 && interp <= 11 then
                                        Just (zoomRect interp)

                                    else
                                        Nothing
                                )

                updatedCurrentInstruction : Maybe { timeStarted : Time.Posix, instruction : Instruction Msg }
                updatedCurrentInstruction =
                    if animationComplete then
                        Nothing

                    else
                        model.currentInstruction
            in
            ( { model
                | currentInstruction = updatedCurrentInstruction
                , zoomRects = zoomRects
              }
            , Cmd.none
            )

        Instruction.RemoveWindow { withId } ->
            let
                updatedUI : UI.UI Msg
                updatedUI =
                    model.ui
                        |> UI.removeObject withId
            in
            ( { model
                | currentInstruction = Nothing
                , ui = updatedUI
              }
            , Cmd.none
            )

        Instruction.CreateWindow { withId, window, rect } ->
            let
                updatedUI : UI.UI Msg
                updatedUI =
                    model.ui
                        |> UI.createObject
                            (UIObject.new
                                { id = withId
                                , rect = rect
                                }
                                |> UIObject.setView
                                    (View.window window)
                                |> UIObject.setDragOptions
                                    { traveling = View.rect MacOS.UI.View.Rectangle.StyleDotted
                                    , preDragInPixels = 0
                                    }
                            )
                        |> UI.attachObject
                            { objectId = withId
                            , parentId = domIds.windows
                            , rect = rect
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

        Instruction.AttachObject { objectId, parentId, rect } ->
            let
                updatedUI : UI.UI Msg
                updatedUI =
                    model.ui
                        |> UI.attachObject
                            { objectId = objectId
                            , parentId = parentId
                            , rect = rect
                            }
            in
            ( { model
                | currentInstruction = Nothing
                , ui = updatedUI
              }
            , Cmd.none
            )

        Instruction.UpdateWindowRect { objectId, rect } ->
            let
                updatedUI : UI.UI Msg
                updatedUI =
                    model.ui
                        |> UI.updateObject objectId (UIObject.setRect rect)
            in
            ( { model
                | currentInstruction = Nothing
                , ui = updatedUI
              }
            , Cmd.none
            )

        Instruction.ReparentObjectToWindow { objectId, windowId, rectInWindow } ->
            let
                maybeObject : Maybe (UIObject.Object Msg)
                maybeObject =
                    UI.getObject model.ui objectId

                updatedUI : UI.UI Msg
                updatedUI =
                    case maybeObject of
                        Just object ->
                            model.ui
                                |> UI.reparentObject
                                    { objectId = objectId
                                    , newParentId = windowId
                                    , newRect = rectInWindow
                                    }

                        Nothing ->
                            model.ui
            in
            ( { model
                | currentInstruction = Nothing
                , ui = updatedUI
              }
            , Cmd.none
            )

        Instruction.UpdateObjectText { objectId, text } ->
            let
                updatedUI : UI.UI Msg
                updatedUI =
                    model.ui
                        |> UI.updateObject objectId (UIObject.setText text)
            in
            ( { model
                | currentInstruction = Nothing
                , ui = updatedUI
              }
            , Cmd.none
            )

        Instruction.UpdateObjectSelected { objectId, selected } ->
            let
                updatedUI : UI.UI Msg
                updatedUI =
                    model.ui
                        |> UI.updateObject objectId (UIObject.setSelected selected)
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
                    UI.hitTest
                        { candidates = Nothing
                        , coordinate = newMousePos
                        }
                        model.ui

                maybePickedObjectId : Maybe String
                maybePickedObjectId =
                    UI.pickTopmostObject hitTestResults model.ui

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

                updatedMouse : Mouse
                updatedMouse =
                    if mouseStateChanged then
                        Mouse.update
                            (Mouse.toMsg domUpdate)
                            model.mouse

                    else
                        model.mouse

                newMouseEvents : List MouseEvent
                newMouseEvents =
                    let
                        maybePickedObject : Maybe (Object Msg)
                        maybePickedObject =
                            Maybe.andThen (UI.getObject model.ui) maybePickedObjectId
                    in
                    if mouseStateChanged then
                        detectMouseEvents
                            model.dragging
                            updatedMouse
                            maybePickedObject
                            model.lastMouseUp
                            model.lastMouseDown
                            model.lastMouseDownObject
                            model.lastClick
                            model.currentTime
                            model.maxTimeBetweenDoubleClicks

                    else
                        []

                mouseEventCmds : Cmd Msg
                mouseEventCmds =
                    if not (Mouse.locked model.mouse) then
                        newMouseEvents
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
                                    | drawRect =
                                        Rect.setPosition
                                            (Coordinate.plus newMousePos dragging.clickOffset)
                                            dragging.drawRect
                                    , dragDelta =
                                        newMousePos
                                            |> Coordinate.minus dragging.absolutePositionAtClick
                                            |> Coordinate.plus dragging.clickOffset
                                }

                        Nothing ->
                            Nothing
            in
            ( { model
                | mouse = updatedMouse
                , dragging = updatedDragging
                , pickedObjectId = maybePickedObjectId
                , debug = maybePickedObjectId
              }
            , mouseEventCmds
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

                maybeMouseEventCmd : Cmd Msg
                maybeMouseEventCmd =
                    Maybe.map sendMsg maybeMsgFromEventHandler
                        |> Maybe.withDefault Cmd.none
            in
            case event of
                MouseEvent.MouseDown objectId ->
                    let
                        updatedModel : Model
                        updatedModel =
                            { model
                                | ui =
                                    UI.updateObject objectId
                                        (UIObject.setSelected True)
                                        model.ui
                                , lastMouseDown =
                                    Just
                                        { objectId = objectId
                                        , time = model.currentTime
                                        , coordinate = Mouse.position model.mouse
                                        }
                                , lastMouseDownObject = UI.getObject model.ui objectId
                            }
                    in
                    ( updatedModel
                    , maybeMouseEventCmd
                    )

                MouseEvent.MouseUp objectId ->
                    let
                        ( updatedApp, fromAppInstructions ) =
                            case model.dragging of
                                Just dragging ->
                                    let
                                        isWindow : Bool
                                        isWindow =
                                            UI.isWindow dragging.objectId model.ui

                                        droppedOnWindow : Maybe ObjectId
                                        droppedOnWindow =
                                            UI.hitTest
                                                { candidates = Just (UI.getWindowIds model.ui)
                                                , coordinate = Mouse.position model.mouse
                                                }
                                                model.ui
                                                |> (\windowIds ->
                                                        UI.pickTopmostObject windowIds model.ui
                                                   )

                                        droppedOnObjects : List ObjectId
                                        droppedOnObjects =
                                            UI.hitTest
                                                { candidates = Nothing
                                                , coordinate = Mouse.position model.mouse
                                                }
                                                model.ui

                                        dropRect : Rect
                                        dropRect =
                                            dragging.drawRect

                                        dropRectInWindow : Rect
                                        dropRectInWindow =
                                            case droppedOnWindow of
                                                Just windowId ->
                                                    UI.getAbsoluteRect model.ui windowId
                                                        |> Maybe.map Rect.position
                                                        |> Maybe.map
                                                            (\windowCoordinates ->
                                                                Rect.minus windowCoordinates dropRect
                                                            )
                                                        |> Maybe.withDefault dropRect

                                                Nothing ->
                                                    dropRect
                                    in
                                    WindSleepers.update
                                        (WindSleepers.ReceivedMsgFromOS
                                            (ToAppMsg.DroppedObject
                                                { objectId = dragging.objectId
                                                , isWindow = isWindow
                                                , droppedOnWindow = droppedOnWindow
                                                , droppedOnObjects = droppedOnObjects
                                                , dropRectAbsolute = dropRect
                                                , dropRectInWindow = dropRectInWindow
                                                , originRect = dragging.originRect
                                                }
                                            )
                                        )
                                        model.app

                                Nothing ->
                                    ( model.app, [] )
                    in
                    ( { model
                        | dragging = Nothing
                        , lastMouseUp = Just { objectId = objectId, time = model.currentTime }
                        , app = updatedApp
                        , instructions = model.instructions ++ fromAppInstructions
                      }
                    , maybeMouseEventCmd
                    )

                MouseEvent.Click objectId ->
                    ( { model
                        | lastClick = Just { objectId = objectId, time = model.currentTime }
                      }
                    , maybeMouseEventCmd
                    )

                MouseEvent.DoubleClick objectId ->
                    ( { model
                        | lastDoubleClick = Just { objectId = objectId, time = model.currentTime }
                      }
                    , maybeMouseEventCmd
                    )

                MouseEvent.DragStart objectId ->
                    let
                        maybeDraggedObject : Maybe (Object Msg)
                        maybeDraggedObject =
                            UI.getObject model.ui objectId

                        maybeDragView : Maybe (View Msg)
                        maybeDragView =
                            Maybe.andThen UIObject.getDragOptions maybeDraggedObject
                                |> Maybe.map .traveling

                        updatedModel : Model
                        updatedModel =
                            case ( maybeDraggedObject, maybeDragView ) of
                                ( Just draggedObject, Just dragView ) ->
                                    let
                                        draggedObjectId : String
                                        draggedObjectId =
                                            UIObject.id draggedObject

                                        draggedObjectAbsoluteRect : Rect
                                        draggedObjectAbsoluteRect =
                                            UI.getAbsoluteRect model.ui draggedObjectId
                                                |> Maybe.withDefault (Rect.new ( 0, 0 ) ( 0, 0 ))
                                                |> Rect.setSize (UIObject.size draggedObject)

                                        mouseOffset : Coordinate
                                        mouseOffset =
                                            let
                                                clickPoint : Coordinate
                                                clickPoint =
                                                    case model.lastMouseDown of
                                                        Just mouseDown ->
                                                            mouseDown.coordinate

                                                        Nothing ->
                                                            Mouse.position model.mouse
                                            in
                                            Coordinate.minus
                                                clickPoint
                                                (Rect.position draggedObjectAbsoluteRect)
                                    in
                                    { model
                                        | dragging =
                                            Just
                                                { objectId = draggedObjectId
                                                , absolutePositionAtClick = Rect.position draggedObjectAbsoluteRect
                                                , clickOffset = mouseOffset
                                                , drawRect = draggedObjectAbsoluteRect
                                                , view = dragView
                                                , dragDelta = Coordinate.new ( 0, 0 )
                                                , originRect = draggedObjectAbsoluteRect
                                                }
                                    }

                                _ ->
                                    model
                    in
                    ( updatedModel
                    , maybeMouseEventCmd
                    )


detectMouseEvents :
    Maybe Dragging
    -> Mouse
    -> Maybe (Object Msg)
    -> Maybe { objectId : ObjectId, time : Time.Posix }
    -> Maybe { objectId : ObjectId, time : Time.Posix, coordinate : Coordinate }
    -> Maybe (Object Msg)
    -> Maybe { objectId : ObjectId, time : Time.Posix }
    -> Time.Posix
    -> Int
    -> List MouseEvent
detectMouseEvents dragging mouse maybePickedObject lastMouseUp lastMouseDown maybeLastMouseDownObject lastMouseClick currentTime maxTimeBetweenDoubleClicks =
    List.filterMap identity
        [ detectMouseDownEvent mouse maybePickedObject
        , detectMouseUpEvent mouse maybePickedObject
        , detectClickEvent mouse maybePickedObject lastMouseDown
        , detectDoubleClickEvent mouse maybePickedObject lastMouseDown lastMouseClick currentTime maxTimeBetweenDoubleClicks
        , detectDragStartEvent dragging mouse maybePickedObject lastMouseUp lastMouseDown
        ]


detectMouseDownEvent : Mouse -> Maybe (Object Msg) -> Maybe MouseEvent
detectMouseDownEvent mouse maybePickedObject =
    case maybePickedObject of
        Just pickedObject ->
            if Mouse.buttonJustPressed mouse then
                Just (MouseEvent.MouseDown (UIObject.id pickedObject))

            else
                Nothing

        Nothing ->
            Nothing


detectMouseUpEvent : Mouse -> Maybe (Object Msg) -> Maybe MouseEvent
detectMouseUpEvent mouse maybePickedObject =
    case maybePickedObject of
        Just pickedObject ->
            if Mouse.buttonJustReleased mouse then
                Just (MouseEvent.MouseUp (UIObject.id pickedObject))

            else
                Nothing

        Nothing ->
            Nothing


detectClickEvent :
    Mouse
    -> Maybe (Object Msg)
    -> Maybe { objectId : ObjectId, time : Time.Posix, coordinate : Coordinate }
    -> Maybe MouseEvent
detectClickEvent mouse maybePickedObject lastMouseDown =
    case maybePickedObject of
        Just pickedObject ->
            if Mouse.buttonJustReleased mouse then
                case lastMouseDown of
                    Just mouseDown ->
                        if mouseDown.objectId == UIObject.id pickedObject then
                            Just (MouseEvent.Click (UIObject.id pickedObject))

                        else
                            Nothing

                    Nothing ->
                        Nothing

            else
                Nothing

        Nothing ->
            Nothing


detectDoubleClickEvent :
    Mouse
    -> Maybe (Object Msg)
    -> Maybe { objectId : ObjectId, time : Time.Posix, coordinate : Coordinate }
    -> Maybe { objectId : ObjectId, time : Time.Posix }
    -> Time.Posix
    -> Int
    -> Maybe MouseEvent
detectDoubleClickEvent mouse maybePickedObject lastMouseDown lastClick currentTime maxTimeBetweenDoubleClicks =
    case maybePickedObject of
        Just pickedObject ->
            if Mouse.buttonJustReleased mouse then
                case lastMouseDown of
                    Just mouseDown ->
                        if mouseDown.objectId == UIObject.id pickedObject then
                            case lastClick of
                                Just click ->
                                    let
                                        timeIsUnderMaxTime : Bool
                                        timeIsUnderMaxTime =
                                            (Time.posixToMillis currentTime - Time.posixToMillis click.time) <= maxTimeBetweenDoubleClicks
                                    in
                                    if click.objectId == mouseDown.objectId && timeIsUnderMaxTime then
                                        Just (MouseEvent.DoubleClick (UIObject.id pickedObject))

                                    else
                                        Nothing

                                Nothing ->
                                    Nothing

                        else
                            Nothing

                    Nothing ->
                        Nothing

            else
                Nothing

        Nothing ->
            Nothing


detectDragStartEvent :
    Maybe Dragging
    -> Mouse
    -> Maybe (Object Msg)
    -> Maybe { objectId : ObjectId, time : Time.Posix }
    -> Maybe { objectId : ObjectId, time : Time.Posix, coordinate : Coordinate }
    -> Maybe MouseEvent
detectDragStartEvent dragging mouse maybeLastMouseDownObject lastMouseUp lastMouseDown =
    let
        buttonIsPressed : Bool
        buttonIsPressed =
            Mouse.buttonPressed mouse

        buttonWasHeldContinuously : Bool
        buttonWasHeldContinuously =
            case ( lastMouseDown, lastMouseUp ) of
                ( Just mouseDown, Just mouseUp ) ->
                    Time.posixToMillis mouseUp.time < Time.posixToMillis mouseDown.time

                ( Just mouseDown, Nothing ) ->
                    True

                ( Nothing, _ ) ->
                    False

        mouseHasMovedPastDragMinimum : Bool
        mouseHasMovedPastDragMinimum =
            case ( lastMouseDown, maybeLastMouseDownObject ) of
                ( Just mouseDown, Just mouseDownObject ) ->
                    let
                        preDragInPixels : Int
                        preDragInPixels =
                            UIObject.getDragOptions mouseDownObject
                                |> Maybe.map .preDragInPixels
                                |> Maybe.withDefault 0
                    in
                    Coordinate.cityBlockDistance (Mouse.position mouse) mouseDown.coordinate >= preDragInPixels

                _ ->
                    False
    in
    case dragging of
        Just _ ->
            Nothing

        Nothing ->
            if buttonIsPressed && buttonWasHeldContinuously && mouseHasMovedPastDragMinimum then
                case maybeLastMouseDownObject of
                    Just mouseDownObject ->
                        Just (MouseEvent.DragStart (UIObject.id mouseDownObject))

                    Nothing ->
                        Nothing

            else
                Nothing


sendMsg : Msg -> Cmd Msg
sendMsg msg =
    Task.perform identity (Task.succeed msg)


view : Model -> Html Msg
view model =
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
        [ UI.view { debugObject = "" } model.ui
        , viewDraggedObject model
        , MenuBar.view (Screen.width model.screen) model.menuBar
        , viewZoomRects model
        , viewScreenCorners (Screen.logical model.screen)
        , viewDebugger model
        , Mouse.view model.mouse
        ]


viewDraggedObject : Model -> Html Msg
viewDraggedObject model =
    case model.dragging of
        Just dragging ->
            View.view dragging.drawRect "" dragging.view []

        Nothing ->
            UIHelpers.none


viewZoomRects : Model -> Html Msg
viewZoomRects model =
    div
        [ style "position" "absolute"
        ]
        (model.zoomRects
            |> List.map
                (\rect ->
                    MacOS.UI.View.Rectangle.view
                        MacOS.UI.View.Rectangle.StyleDotted
                        rect
                        []
                )
        )


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
