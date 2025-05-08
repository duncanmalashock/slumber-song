module MacOS.Visible exposing (Visible, image, rect, view, window)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.Visible.Image
import MacOS.Visible.Rect
import MacOS.Window


type Visible msg
    = Rect MacOS.Visible.Rect.Config
    | Window (MacOS.Window.Window msg)
    | Image { url : String, size : ( Int, Int ) }


rect : MacOS.Visible.Rect.Config -> Visible msg
rect params =
    Rect params


window : MacOS.Window.Window msg -> Visible msg
window params =
    Window params


image : { url : String, size : ( Int, Int ) } -> Visible msg
image params =
    Image params


view : Rect -> Visible msg -> Html msg
view objRect screenObject =
    case screenObject of
        Rect params ->
            MacOS.Visible.Rect.draw params objRect

        Window params ->
            MacOS.Window.view params (True |> Debug.log "TODO")

        Image params ->
            MacOS.Visible.Image.view params objRect
