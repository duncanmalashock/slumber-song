module MacOS.UI.View exposing
    ( View
    , rect, image, window
    , view
    )

{-| The visual representation of a UI Object.

@docs View


# Kinds of views

@docs rect, image, window


# Convert to HTML

@docs view

-}

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.UI.View.Image
import MacOS.UI.View.Rect
import MacOS.UI.View.Window


type View msg
    = Rect MacOS.UI.View.Rect.Config
    | Window (MacOS.UI.View.Window.Window msg)
    | Image { url : String, size : ( Int, Int ) }


rect : MacOS.UI.View.Rect.Config -> View msg
rect params =
    Rect params


window : MacOS.UI.View.Window.Window msg -> View msg
window params =
    Window params


image : { url : String, size : ( Int, Int ) } -> View msg
image params =
    Image params


view : Rect -> View msg -> List (Html msg) -> Html msg
view objRect objectView childrenViews =
    case objectView of
        Rect params ->
            MacOS.UI.View.Rect.view params objRect childrenViews

        Window params ->
            MacOS.UI.View.Window.view params True objRect childrenViews

        Image params ->
            MacOS.UI.View.Image.view params objRect childrenViews
