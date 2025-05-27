module MacOS.MouseEvent exposing (MouseEvent(..))


type MouseEvent
    = MouseDown String
    | MouseUp String
    | Click String
    | DoubleClick String
    | DragStart String
