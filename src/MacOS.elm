module MacOS exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.FileSystem as FileSystem exposing (FileSystem)
import MacOS.FillPattern as FillPattern
import MacOS.MenuBar as MenuBar exposing (MenuBar)
import MacOS.Mouse as Mouse exposing (Mouse)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.Screen as Screen exposing (Screen)
import MacOS.ViewHelpers as ViewHelpers exposing (imgURL, px)
import MacOS.Window as Window exposing (Window)


type alias Model =
    { active : Maybe String
    , dragging : Maybe Window.DragInfo
    , windows : List Window
    , screen : Screen
    , menuBar : MenuBar
    , fileSystem : FileSystem
    , mouse : Mouse
    }


type alias Flags =
    { browserDimensions : { x : Int, y : Int }
    , devicePixelRatio : Float
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { active = Nothing
      , dragging = Nothing
      , windows =
            []
      , screen =
            Screen.new
                { screen = Coordinate.new ( 512, 342 )
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
                [ FileSystem.volume "Diskette" []
                ]
      , mouse = Mouse.new
      }
    , Cmd.none
    )


type Msg
    = ClickedWindow Window
    | ClickedDesktop
    | PointerDownWindowTitle Window.DragInfo
    | PointerMove Coordinate
    | PointerUp
    | ClickedDisk FileSystem.Volume
    | ClickedWindowCloseBox
    | MouseMsg Mouse.Msg
    | BrowserResized Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedWindow window ->
            ( { model | active = Just window.title, windows = bringToFront window model.windows }, Cmd.none )

        ClickedDesktop ->
            ( { model | active = Nothing }, Cmd.none )

        PointerDownWindowTitle dragInfo ->
            ( { model | dragging = Just dragInfo }, Cmd.none )

        PointerMove newCursorCoordinate ->
            ( { model
                | dragging =
                    case model.dragging of
                        Just dragInfo ->
                            Just { dragInfo | cursor = newCursorCoordinate }

                        Nothing ->
                            Nothing
              }
            , Cmd.none
            )

        PointerUp ->
            case model.dragging of
                Just dragInfo ->
                    ( { model
                        | dragging = Nothing
                        , windows =
                            model.windows
                                |> moveDraggedWindow dragInfo
                                |> bringToFront dragInfo.window
                        , active = Just dragInfo.window.title
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | dragging = Nothing }, Cmd.none )

        ClickedDisk volume ->
            let
                name =
                    FileSystem.name volume
            in
            ( { model
                | windows =
                    model.windows
                        ++ [ { title = name
                             , rect = Rect.new ( 50, 50 ) ( 200, 150 )
                             }
                           ]
                , active = Just name
              }
            , Cmd.none
            )

        ClickedWindowCloseBox ->
            ( { model
                | windows = []
              }
            , Cmd.none
            )

        MouseMsg mouseMsg ->
            ( { model
                | mouse = Mouse.update mouseMsg model.mouse
              }
            , Cmd.none
            )

        BrowserResized newWidth newHeight ->
            ( { model
                | screen = Screen.update { x = newWidth, y = newHeight } model.screen
              }
            , Cmd.none
            )


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


bringToFront : Window -> List Window -> List Window
bringToFront target windows =
    windows
        |> List.sortWith
            (\a b ->
                if a.title == target.title then
                    GT

                else if b.title == target.title then
                    LT

                else
                    EQ
            )


viewDebugger : Model -> Html Msg
viewDebugger model =
    div
        [ style "position" "absolute"
        , style "z-index" "2"
        , style "top" (px 128)
        , style "left" (px 16)
        , style "width" (px 480)
        ]
        [ div
            [ style "background-color" "black"
            , style "color" "white"
            , style "font-family" "Geneva"
            , style "padding" "0 6px"
            ]
            [ div [] [ text <| Debug.toString model.screen ]
            , div [] [ text <| Debug.toString model.mouse ]
            ]
        ]


view : Model -> Html Msg
view model =
    div
        ([ style "width" (px (Screen.width model.screen))
         , style "height" (px (Screen.height model.screen))
         , style "background-color" "black"
         , style "background-image" FillPattern.dither50
         , style "position" "relative"
         , Screen.scaleAttr model.screen

         -- , Events.on "pointerdown" (Decode.succeed ClickedDesktop)
         , style "overflow" "hidden"
         ]
            ++ Mouse.eventsForBaseElement model.screen MouseMsg
        )
        [ viewDebugger model
        , MenuBar.view (Screen.width model.screen) model.menuBar
        , viewScreenCorners (Screen.logical model.screen)
        , div []
            (model.fileSystem
                |> FileSystem.volumes
                |> List.map viewVolume
            )
        , model.windows
            |> List.map
                (\window ->
                    Window.view
                        ClickedWindowCloseBox
                        PointerDownWindowTitle
                        ClickedWindow
                        (model.active == Just window.title)
                        window
                )
            |> div []
        , case model.dragging of
            Just info ->
                div []
                    [ dragOutline
                        { size = Rect.size info.window.rect
                        , position =
                            info.cursor
                                |> Coordinate.minus info.offset
                        }
                    ]

            _ ->
                ViewHelpers.none
        ]


viewVolume : FileSystem.Volume -> Html Msg
viewVolume volume =
    div
        [ style "position" "absolute"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "top" (px 32)
        , style "left" (px 442)
        , onClick (ClickedDisk volume)
        ]
        [ div
            [ style "width" (px 32)
            , style "height" (px 32)
            , style "background-image" (imgURL "MacOS/disk.gif")
            ]
            []
        , div
            [ style "height" (px 12)
            , style "text-align" "center"
            , style "top" (px 32)
            , style "left" (px -23)
            , style "background-color" "white"
            , style "font-family" "Geneva"
            , style "line-height" (px 11)
            , style "padding" "0 2px"
            ]
            [ text (FileSystem.name volume) ]
        ]


dragOutline : { size : Coordinate, position : Coordinate } -> Html msg
dragOutline { size, position } =
    div
        [ style "position" "fixed"
        , style "z-index" "1"
        , style "top" (px (Coordinate.y position - 1))
        , style "left" (px (Coordinate.x position))
        , style "width" (px (Coordinate.x size))
        , style "height" (px (Coordinate.y size))
        , style "pointer-events" "none"
        , style "mix-blend-mode" "multiply"
        , style "background-image" FillPattern.dither50
        ]
        [ div
            [ style "position" "relative"
            , style "top" (px 1)
            , style "left" (px 1)
            , style "width" (px (Coordinate.x size - 2))
            , style "height" (px (Coordinate.y size - 2))
            , style "background" "white"
            ]
            []
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
    Browser.Events.onResize (\width height -> BrowserResized width height)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
