module MacOS.Instruction exposing (Instruction(..))

{-| An instruction to be carried out by the operating system.


# Instruction

@docs Instruction

-}

import MacOS.Rect as Rect exposing (Rect)
import MacOS.UI.Object as UIObject exposing (Object)
import MacOS.UI.View.Window as Window exposing (Window)


type Instruction msg
    = AnimateZoom { from : Rect, to : Rect, zoomingIn : Bool }
    | CreateWindow { withId : String, window : Window msg }
    | RemoveWindow { withId : String }
    | CreateObject { object : Object msg }
    | AttachObject { objectId : String, parentId : String }
