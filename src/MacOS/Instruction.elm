module MacOS.Instruction exposing (Instruction(..))

import MacOS.Rect as Rect exposing (Rect)
import MacOS.UIObject as UIObject exposing (UIObject)
import MacOS.Window as Window exposing (Window)


type Instruction msg
    = AnimateZoom { from : Rect, to : Rect, zoomingIn : Bool }
    | CreateWindow { withId : String, window : Window msg }
    | RemoveWindow { withId : String }
    | CreateObject { withId : String, object : UIObject msg }
    | AttachObject { layerId : String, objectId : String }
