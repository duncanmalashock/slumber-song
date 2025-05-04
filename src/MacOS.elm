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
import MacOS.Interface as Interface exposing (Interface)
import MacOS.MenuBar as MenuBar exposing (MenuBar)
import MacOS.Mouse as Mouse exposing (Mouse)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.Screen as Screen exposing (Screen)
import MacOS.UIObject as UIObject exposing (UIObject)
import MacOS.ViewHelpers as ViewHelpers exposing (imgURL, px)
import MacOS.Window as Window exposing (Window)
import Set
import Task
import Time


viewDebugger : Model -> Html Msg
viewDebugger model =
    div
        [ style "position" "absolute"
        , style "z-index" "2"
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
            [ div [] [ text <| Mouse.debug model.mouse ]
            ]
        ]


type alias Model =
    { currentTime : Time.Posix
    , screen : Screen
    , menuBar : MenuBar
    , fileSystem : FileSystem
    , mouse : Mouse
    , interface : Interface
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
      , interface =
            Interface.new
                |> Interface.add
                    [ ( "Prickly Pete"
                      , UIObject.new
                            { rect = Rect.new ( 96, 96 ) ( 128, 128 )
                            }
                      )
                    , ( "Snoopy"
                      , UIObject.new
                            { rect = Rect.new ( 196, 64 ) ( 128, 128 )
                            }
                      )
                    ]
      }
    , Cmd.none
    )


type Msg
    = Tick Time.Posix
    | MouseMsg { clientPos : ( Int, Int ), buttonClicked : Bool }
    | BrowserResized Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model
                | currentTime = time
                , screen = Screen.firstPaintDone model.screen
              }
            , Cmd.none
            )

        MouseMsg args ->
            let
                newMousePos =
                    Coordinate.new args.clientPos
                        |> Screen.toScreenCoordinates model.screen

                mouseMsgData : Mouse.MsgData
                mouseMsgData =
                    { atTime = model.currentTime
                    , mouseButtonPressed = args.buttonClicked
                    , position = newMousePos
                    , overObjIds = Interface.containingCoordinate newMousePos model.interface
                    }

                updatedMouse : Mouse
                updatedMouse =
                    Mouse.update
                        (Mouse.toMsg mouseMsgData)
                        model.mouse

                --     mouseEventstoMsgs : Mouse.Event -> List Msg
                --     mouseEventstoMsgs mouseEvent =
                --         case mouseEvent of
                --             Mouse.Click _ { id } ->
                --                 Event.eventToMsgList id Event.Click model.eventRegistry
                --             Mouse.DoubleClick _ { id } ->
                --                 Event.eventToMsgList id Event.DoubleClick model.eventRegistry
                --             Mouse.DragStart _ { id } ->
                --                 Event.eventToMsgList id Event.DragStart model.eventRegistry
                --             Mouse.DragEnd _ { id } ->
                --                 Event.eventToMsgList id Event.DragEnd model.eventRegistry
                --     eventCmds : Cmd Msg
                --     eventCmds =
                --         newMouseEvents
                --             |> List.concatMap mouseEventstoMsgs
                --             |> List.map sendMsg
                --             |> Cmd.batch
            in
            ( { model
                | mouse = updatedMouse
              }
              -- , eventCmds
            , Cmd.none
            )

        BrowserResized newWidth newHeight ->
            ( { model
                | screen = Screen.update { x = newWidth, y = newHeight } model.screen
              }
            , Cmd.none
            )


sendMsg : Msg -> Cmd Msg
sendMsg msg =
    Task.perform identity (Task.succeed msg)


moveDraggedWindow : Window.DragInfo -> List Window -> List Window
moveDraggedWindow info windows =
    windows
        |> List.map
            (\window ->
                if window.title == info.window.title then
                    { window
                        | rect =
                            window.rect
                                |> Rect.addPosition (info.cursor |> Coordinate.minus info.cursorAtDragStart)
                    }

                else
                    window
            )


bringToFront : String -> List Window -> List Window
bringToFront objectId windows =
    windows
        |> List.sortWith
            (\a b ->
                if a.title == objectId then
                    GT

                else if b.title == objectId then
                    LT

                else
                    EQ
            )


type SubEvent
    = MouseMove
    | MouseUp
    | MouseDown


subEventsRequiredForEventType : Event -> List SubEvent
subEventsRequiredForEventType event =
    case event of
        Event.DoubleClick ->
            [ MouseDown
            , MouseUp
            ]

        Event.Click ->
            [ MouseDown
            , MouseUp
            ]

        Event.DragStart ->
            [ MouseDown
            , MouseMove
            ]

        Event.DragEnd ->
            [ MouseMove
            , MouseUp
            ]



-- listenersForObject :
--     Model
--     -> { id : String, coordinate : Coordinate }
--     -> List (Attribute Msg)
-- listenersForObject model { id, coordinate } =
--     let
--         toListeners : SubEvent -> List (Attribute Msg)
--         toListeners subEvent =
--             case subEvent of
--                 MouseMove ->
--                     []
--                 MouseUp ->
--                     [ Mouse.onMouseUpForObject id coordinate model.screen model.currentTime MouseMsg
--                     ]
--                 MouseDown ->
--                     [ Mouse.onMouseDownForObject id coordinate model.screen model.currentTime MouseMsg
--                     ]
--     in
--     Event.listForObject id model.eventRegistry
--         |> List.concatMap subEventsRequiredForEventType
--         |> List.concatMap toListeners


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
         ]
            ++ Mouse.eventsForDesktop MouseMsg
            ++ Screen.scaleAttrs model.screen
        )
        [ viewDesktopObjects model
        , viewDesktopRectangles model
        , viewWindows model
        , viewWindowRectangles model
        , MenuBar.view (Screen.width model.screen) model.menuBar
        , viewDialogs model
        , viewScreenCorners (Screen.logical model.screen)
        , viewDebugger model
        , viewCursor model
        ]


viewDesktopObjects : Model -> Html msg
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


viewDialogs : Model -> Html msg
viewDialogs model =
    ViewHelpers.none


viewCursor : Model -> Html msg
viewCursor model =
    ViewHelpers.none


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
        [ Time.every 50 Tick
        , Browser.Events.onResize BrowserResized
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
