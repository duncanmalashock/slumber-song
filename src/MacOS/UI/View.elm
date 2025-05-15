module MacOS.UI.View exposing
    ( View
    , rect, image, textarea, window
    , view
    )

{-| The visual representation of a UI Object.

@docs View


# Kinds of views

@docs rect, image, textarea, window


# Convert to HTML

@docs view

-}

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.UI.View.Image
import MacOS.UI.View.Rectangle
import MacOS.UI.View.Textarea
import MacOS.UI.View.Window


type View msg
    = Rectangle MacOS.UI.View.Rectangle.Config
    | Window (MacOS.UI.View.Window.Config msg)
    | Image MacOS.UI.View.Image.Config
    | Textarea MacOS.UI.View.Textarea.Config


rect : MacOS.UI.View.Rectangle.Config -> View msg
rect config =
    Rectangle config


window : MacOS.UI.View.Window.Config msg -> View msg
window config =
    Window config


image : MacOS.UI.View.Image.Config -> View msg
image config =
    Image config


textarea : MacOS.UI.View.Textarea.Config -> View msg
textarea config =
    Textarea config


view : Rect -> String -> View msg -> List (Html msg) -> Html msg
view objectRect textContent objectView childrenViews =
    case objectView of
        Rectangle config ->
            MacOS.UI.View.Rectangle.view config objectRect childrenViews

        Window config ->
            MacOS.UI.View.Window.view config True objectRect childrenViews

        Image config ->
            MacOS.UI.View.Image.view config objectRect childrenViews

        Textarea config ->
            MacOS.UI.View.Textarea.view config objectRect textContent childrenViews
