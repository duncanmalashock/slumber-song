module MacOS.Instruction exposing (Instruction(..))

import MacOS.Rect as Rect exposing (Rect)


type Instruction
    = AnimateZoom { from : Rect, to : Rect, zoomingIn : Bool }
    | CreateWindow { withId : String, at : Rect }
    | RemoveWindow { withId : String }
