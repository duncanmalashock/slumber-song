module MacOS exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import MacOS.Context as Context exposing (Context)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Event as Event exposing (Event)
import MacOS.FileSystem as FileSystem exposing (FileSystem)
import MacOS.FillPattern as FillPattern
import MacOS.Instruction as Instruction exposing (Instruction)
import MacOS.Interface as Interface exposing (Interface)
import MacOS.MenuBar as MenuBar exposing (MenuBar)
import MacOS.Mouse as Mouse exposing (Mouse)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.Screen as Screen exposing (Screen)
import MacOS.UIObject as UIObject exposing (UIObject)
import MacOS.ViewHelpers as ViewHelpers exposing (imgURL, px)
import MacOS.Visible as Visible exposing (Visible)
import MacOS.Visible.Rect
import MacOS.Window as Window
import Set
import Task
import Time


viewDebugger : Model -> Html Msg
viewDebugger model =
    div
        [ style "position" "absolute"
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
            [-- div [] [ text <| Debug.toString model.currentInstruction ]
            ]
        ]


type alias Model =
    { currentTime : Time.Posix
    , screen : Screen
    , menuBar : MenuBar
    , fileSystem : FileSystem
    , mouse : Mouse
    , cursor : Maybe Cursor
    , interface : Interface Msg
    , dragging : Maybe Dragging
    , instructions : List Instruction
    , currentInstruction : Maybe { timeStarted : Time.Posix, instruction : Instruction }
    }


type Cursor
    = CursorPointer
    | CursorWatch


type alias Dragging =
    { objId : String
    , rect : Rect
    , offset : Coordinate
    , visible : Visible Msg
    }


type alias Flags =
    { browserDimensions : { x : Int, y : Int }
    , devicePixelRatio : Float
    , currentTimeInMS : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        from =
            Rect.new ( 32, 64 ) ( 350, 250 )

        to =
            Rect.new ( 450, 40 ) ( 32, 32 )
    in
    ( { currentTime = Time.millisToPosix flags.currentTimeInMS
      , screen =
            Screen.new
                { screenInPixels = ( 512, 342 )
                , browser = flags.browserDimensions
                , devicePixelRatio = flags.devicePixelRatio
                }
      , menuBar =
            MenuBar.new
                [ MenuBar.menu "File" False
                , MenuBar.menu "Edit" False
                , MenuBar.menu "View" False
                , MenuBar.menu "Special" False
                ]
      , fileSystem =
            FileSystem.new
                [ FileSystem.volume "disk" []
                ]
      , mouse = Mouse.new
      , cursor = Just CursorPointer
      , interface =
            Interface.new
                |> Interface.addLayer
                    { id = "desktop"
                    , orderConstraint = Just Interface.AlwaysFirst
                    }
                |> Interface.addLayer
                    { id = "desktopRects"
                    , orderConstraint = Nothing
                    }
                |> Interface.addToLayer "desktopRects"
                    [ ( "zoomRect0"
                      , UIObject.new
                            { rect = Rect.new ( 0, 0 ) ( 0, 0 )
                            }
                            |> UIObject.visible
                                (Visible.rect MacOS.Visible.Rect.StyleDotted)
                      )
                    , ( "zoomRect1"
                      , UIObject.new
                            { rect = Rect.new ( 0, 0 ) ( 0, 0 )
                            }
                            |> UIObject.visible
                                (Visible.rect MacOS.Visible.Rect.StyleDotted)
                      )
                    , ( "zoomRect2"
                      , UIObject.new
                            { rect = Rect.new ( 0, 0 ) ( 0, 0 )
                            }
                            |> UIObject.visible
                                (Visible.rect MacOS.Visible.Rect.StyleDotted)
                      )
                    , ( "zoomRect3"
                      , UIObject.new
                            { rect = Rect.new ( 0, 0 ) ( 0, 0 )
                            }
                            |> UIObject.visible
                                (Visible.rect MacOS.Visible.Rect.StyleDotted)
                      )
                    ]
                |> Interface.addLayer
                    { id = "windows"
                    , orderConstraint = Nothing
                    }
                |> Interface.addToLayer "desktop"
                    [ ( "Disk"
                      , UIObject.new
                            { rect = Rect.new ( 450, 40 ) ( 32, 32 )
                            }
                            |> UIObject.visible
                                (Visible.rect MacOS.Visible.Rect.StyleSolidFilled)
                            |> UIObject.selectable
                                { selected = False
                                , view = Visible.rect MacOS.Visible.Rect.StyleFillBlack
                                }
                            |> UIObject.draggable
                                { traveling = Visible.rect MacOS.Visible.Rect.StyleDotted
                                }
                            |> UIObject.onMouseDown ClickedDisk
                      )
                    ]
      , dragging = Nothing
      , instructions = []
      , currentInstruction = Nothing
      }
    , Cmd.none
    )


type Msg
    = Tick Time.Posix
    | BrowserResized Int Int
    | MouseUpdated { clientPos : ( Int, Int ), buttonPressed : Bool }
    | MouseEvent Mouse.Event
    | ClickedDisk
    | ClickedWindow


handleInstruction : { timeStarted : Time.Posix, instruction : Instruction } -> Model -> ( Model, Cmd Msg )
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
                                        clamped =
                                            (i + (animationPhase - 4))
                                                |> Basics.clamp 0 11
                                    in
                                    ( "zoomRect" ++ String.fromInt i
                                    , zoomRect clamped
                                    )
                                )

                updatedInterface : Interface Msg
                updatedInterface =
                    zoomRects
                        |> List.map (\( key, rect ) -> ( key, UIObject.setRect rect ))
                        |> (\updaters -> Interface.updateList updaters model.interface)

                updatedCurrentInstruction =
                    if animationComplete then
                        Nothing

                    else
                        model.currentInstruction
            in
            ( { model
                | currentInstruction = updatedCurrentInstruction
                , interface = updatedInterface
              }
            , Cmd.none
            )

        Instruction.RemoveWindow { withId } ->
            let
                updatedInterface =
                    model.interface
                        |> Interface.remove "window1"
            in
            ( { model
                | currentInstruction = Nothing
                , interface = updatedInterface
              }
            , Cmd.none
            )

        Instruction.CreateWindow { withId, at, title } ->
            let
                updatedInterface =
                    model.interface
                        |> Interface.addToLayer "windows"
                            [ ( withId
                              , UIObject.new
                                    { rect = at
                                    }
                                    |> UIObject.visible
                                        (Visible.window
                                            { title = title
                                            , isActive = True
                                            , closeMsg = ClickedWindow
                                            }
                                        )
                                    |> UIObject.draggable
                                        { traveling = Visible.rect MacOS.Visible.Rect.StyleDotted
                                        }
                              )
                            ]
            in
            ( { model
                | currentInstruction = Nothing
                , interface = updatedInterface
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
                                , cursor = Just CursorPointer
                              }
                            , Cmd.none
                            )

                        first :: rest ->
                            ( { model
                                | currentTime = time
                                , cursor = Just CursorWatch
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
                    { model
                        | currentTime = time
                    }
                        |> handleInstruction
                            { timeStarted = timeStarted
                            , instruction = instruction
                            }

        BrowserResized newWidth newHeight ->
            ( { model
                | screen =
                    model.screen
                        |> Screen.firstPaintDone
                        |> Screen.update { x = newWidth, y = newHeight }
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
                    Interface.containingCoordinate newMousePos model.interface

                mouseMsgData : Mouse.MsgData
                mouseMsgData =
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
                            (Mouse.toMsg mouseMsgData)
                            model.mouse

                    else
                        ( model.mouse, [] )

                pickedId : Maybe String
                pickedId =
                    Interface.topmostFromList hitTestResults model.interface

                eventCmds : Cmd Msg
                eventCmds =
                    case model.cursor of
                        Just CursorPointer ->
                            newMouseEvents
                                |> Mouse.filterEventsByObjId pickedId
                                |> List.map MouseEvent
                                |> List.map sendMsg
                                |> Cmd.batch

                        _ ->
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

        ClickedDisk ->
            ( { model
                | dragging = Nothing
                , instructions =
                    model.instructions
                        ++ [ Instruction.AnimateZoom
                                { from = Rect.new ( 450, 40 ) ( 32, 32 )
                                , to = Rect.new ( 64, 64 ) ( 200, 200 )
                                , zoomingIn = True
                                }
                           , Instruction.CreateWindow
                                { withId = "window1"
                                , at = Rect.new ( 64, 64 ) ( 200, 200 )
                                , title = "window"
                                }
                           ]
              }
            , Cmd.none
            )

        ClickedWindow ->
            ( { model
                | dragging = Nothing
                , instructions =
                    model.instructions
                        ++ [ Instruction.RemoveWindow { withId = "window1" }
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
                    Interface.msgForMouseEvent event model.interface

                cmd : Cmd Msg
                cmd =
                    Maybe.map sendMsg maybeMsgFromEventHandler
                        |> Maybe.withDefault Cmd.none
            in
            case event of
                Mouse.MouseDown objId ->
                    let
                        updatedModel =
                            { model
                                | interface =
                                    Interface.update objId
                                        (UIObject.setSelected True)
                                        model.interface
                            }
                    in
                    ( updatedModel
                    , cmd
                    )

                Mouse.MouseUp ->
                    let
                        updatedInterface =
                            case model.dragging of
                                Just dragging ->
                                    Interface.update dragging.objId
                                        (UIObject.setPosition (Rect.position dragging.rect))
                                        model.interface

                                Nothing ->
                                    model.interface
                    in
                    ( { model
                        | dragging = Nothing
                        , interface = updatedInterface
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
                        maybeDraggedObject : Maybe (UIObject Msg)
                        maybeDraggedObject =
                            Interface.get objId model.interface

                        maybeDragVis : Maybe (Visible Msg)
                        maybeDragVis =
                            Maybe.andThen
                                UIObject.getDraggable
                                maybeDraggedObject
                                |> Maybe.map .traveling

                        updatedModel : Model
                        updatedModel =
                            case ( maybeDraggedObject, maybeDragVis ) of
                                ( Just obj, Just dragVis ) ->
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
                                                , visible = dragVis
                                                }
                                        , interface = Interface.bringObjectToFront objId model.interface
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
       Interface Layers
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
        [ viewDesktopObjects model
        , viewDesktopRectangles model
        , viewWindows model
        , viewWindowRectangles model
        , viewDraggedObject model
        , MenuBar.view (Screen.width model.screen) model.menuBar
        , viewDialogs model
        , viewScreenCorners (Screen.logical model.screen)
        , viewDebugger model
        , viewCursor model
        ]


viewDesktopObjects : Model -> Html Msg
viewDesktopObjects model =
    Interface.view model.interface


viewDesktopRectangles : Model -> Html msg
viewDesktopRectangles model =
    ViewHelpers.none


viewWindows : Model -> Html msg
viewWindows model =
    ViewHelpers.none


viewWindowRectangles : Model -> Html msg
viewWindowRectangles model =
    ViewHelpers.none


viewDraggedObject : Model -> Html Msg
viewDraggedObject model =
    case model.dragging of
        Just dragging ->
            Visible.view dragging.rect dragging.visible

        Nothing ->
            ViewHelpers.none


viewDialogs : Model -> Html msg
viewDialogs model =
    ViewHelpers.none


viewCursor : Model -> Html msg
viewCursor model =
    let
        cursorData =
            case model.cursor of
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
                    , offsetX = 3
                    , offsetY = 3
                    }
    in
    div
        [ style "position" "relative"
        , style "left" (px (Mouse.x model.mouse + cursorData.offsetX))
        , style "top" (px (Mouse.y model.mouse + cursorData.offsetY))
        , style "width" (px 16)
        , style "height" (px 16)
        , style "background-image" (imgURL cursorData.image)
        , style "pointer-events" "none"
        ]
        []


viewVolume : Context Msg -> Maybe String -> FileSystem.Volume -> Html Msg
viewVolume context activeFile volume =
    let
        coordinate : Coordinate
        coordinate =
            Coordinate.new ( 442, 32 )
    in
    div
        ([ style "position" "absolute"
         , style "display" "flex"
         , style "flex-direction" "column"
         , style "align-items" "center"
         , style "top" (px 32)
         , style "left" (px 442)
         ]
            ++ context.listenersForObject
                { id = "disk"
                , coordinate = coordinate
                }
        )
        [ div
            ([ style "width" (px 32)
             , style "height" (px 32)
             , style "background-image" (imgURL "MacOS/disk.gif")
             ]
                ++ (case activeFile of
                        Just "disk" ->
                            [ style "filter" "invert(1)" ]

                        _ ->
                            []
                   )
            )
            []
        , div
            ([ style "height" (px 12)
             , style "text-align" "center"
             , style "top" (px 32)
             , style "left" (px -23)
             , style "background-color" "white"
             , style "font-family" "Geneva"
             , style "line-height" (px 11)
             , style "padding" "0 2px"
             ]
                ++ (case activeFile of
                        Just "disk" ->
                            [ style "filter" "invert(1)" ]

                        _ ->
                            []
                   )
            )
            [ text (FileSystem.name volume) ]
        ]


type Corner
    = TopLeftCorner
    | TopRightCorner
    | BottomLeftCorner
    | BottomRightCorner


viewScreenCorners : Rect -> Html msg
viewScreenCorners screen =
    let
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
        [ style "width" (px (Rect.width screen))
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
