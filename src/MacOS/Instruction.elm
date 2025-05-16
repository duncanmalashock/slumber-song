module MacOS.Instruction exposing (Instruction(..))

{-| An instruction to be carried out by the operating system.


# Instruction

@docs Instruction

-}

import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.UI.Object as UIObject exposing (Object)
import MacOS.UI.View.Window as Window


type Instruction msg
    = AnimateZoom { from : Rect, to : Rect, zoomingIn : Bool }
    | CreateWindow { withId : String, window : Window.Config msg, rect : Rect }
    | RemoveWindow { withId : String }
    | CreateObject { object : Object msg }
    | AttachObject { objectId : String, parentId : String, rect : Rect }
    | UpdateWindowRect { objectId : String, rect : Rect }
    | ReparentObjectToWindow { objectId : String, windowId : String, rectInWindow : Rect }
    | UpdateObjectText { objectId : String, text : String }
    | UpdateObjectSelected { objectId : String, selected : Bool }
