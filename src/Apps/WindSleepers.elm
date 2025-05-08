module Apps.WindSleepers exposing (program)

import MacOS.Instruction as Instruction exposing (Instruction)
import MacOS.Rect as Rect
import MacOS.UI.Object as UIObject
import MacOS.UI.View.Window as Window


type Msg
    = NoOp


program : List (Instruction msg)
program =
    [ Instruction.CreateWindow
        { withId = "scene"
        , window =
            { title = "Temple Ruins"
            , closeMsg = Nothing
            , rect = Rect.new ( 128, 64 ) ( 256, 172 )
            }
        }
    , Instruction.CreateObject
        { withId = "scene:ruins"
        , object =
            UIObject.image
                { url = "WindSleepers/ruins.gif"
                , size = ( 256, 172 )
                }
        }
    , Instruction.AttachObject
        { objectId = "scene:ruins"
        , layerId = "windows"
        }
    ]
