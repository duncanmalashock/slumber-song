module MacOS exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.FillPattern as FillPattern
import MacOS.MenuBar as MenuBar
import MacOS.Rect as Rect exposing (Rect)
import MacOS.ViewHelpers as ViewHelpers exposing (px)
import MacOS.Window as Window exposing (Window)


type alias Model =
    { active : Maybe String
    , dragging : Maybe Window.DragInfo
    , windows : List Window
    }


init : Model
init =
    { active = Just "Entrance"
    , dragging = Nothing
    , windows =
        [ Window "Inventory" (Rect.new ( 25, 50 ) ( 150, 150 ))
        , Window "Entrance" (Rect.new ( 200, 150 ) ( 200, 125 ))
        ]
    }


type Msg
    = ClickedWindow Window
    | ClickedDesktop
    | PointerDownWindowTitle Window.DragInfo
    | PointerMove Coordinate
    | PointerUp


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedWindow window ->
            { model | active = Just window.title, windows = bringToFront window model.windows }

        ClickedDesktop ->
            { model | active = Nothing }

        PointerDownWindowTitle dragInfo ->
            { model | dragging = Just dragInfo }

        PointerMove newCursorCoordinate ->
            { model
                | dragging =
                    case model.dragging of
                        Just dragInfo ->
                            Just { dragInfo | cursor = newCursorCoordinate }

                        Nothing ->
                            Nothing
            }

        PointerUp ->
            case model.dragging of
                Just dragInfo ->
                    { model
                        | dragging = Nothing
                        , windows =
                            model.windows
                                |> moveDraggedWindow dragInfo
                                |> bringToFront dragInfo.window
                        , active = Just dragInfo.window.title
                    }

                Nothing ->
                    { model | dragging = Nothing }


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


view : Model -> Html Msg
view model =
    div
        [ style "width" "512px"
        , style "height" "342px"
        , style "background-color" "white"
        , style "background-image" FillPattern.dither50
        , style "position" "relative"
        , Events.on "pointerdown" (Decode.succeed ClickedDesktop)
        , onPointerMove PointerMove
        , onPointerUp PointerUp
        , style "overflow" "hidden"
        ]
        [ MenuBar.view
        , model.windows
            |> List.map
                (\window ->
                    Window.view
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
                        , position = info.cursor |> Coordinate.minus info.offset
                        }
                    ]

            _ ->
                ViewHelpers.none
        ]


dragOutline : { size : Coordinate, position : Coordinate } -> Html msg
dragOutline { size, position } =
    div
        [ style "position" "fixed"
        , style "z-index" "1"
        , style "top" (px (Coordinate.y position))
        , style "left" (px (Coordinate.x position))
        , style "width" (px (Coordinate.x size))
        , style "height" (px (Coordinate.y size))
        , style "pointer-events" "none"
        , style "mix-blend-mode" "difference"
        , style "border" "dotted 1px #fff"
        ]
        []


onPointerMove : (Coordinate -> msg) -> Attribute msg
onPointerMove toMsg =
    Events.on "pointermove"
        (Decode.map2
            (\cx cy ->
                toMsg (Coordinate.new ( cx, cy ))
            )
            (Decode.field "clientX" ViewHelpers.roundFloat)
            (Decode.field "clientY" ViewHelpers.roundFloat)
        )


onPointerUp : msg -> Attribute msg
onPointerUp msg =
    Events.on "pointerup" (Decode.succeed msg)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
