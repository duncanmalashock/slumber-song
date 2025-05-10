module Apps.WindSleepers exposing (program)

import MacOS.Instruction as Instruction exposing (Instruction)
import MacOS.Rect as Rect
import MacOS.UI.Helpers as UIHelpers exposing (domIds)
import MacOS.UI.Object as UIObject
import MacOS.UI.View.Window as Window


program : List (Instruction msg)
program =
    [ Instruction.CreateWindow
        { withId = "scene"
        , window =
            { title = "Temple Ruins"
            , closeMsg = Nothing
            }
        , rect = Rect.new ( 128, 64 ) ( 256, 172 )
        }
    , Instruction.CreateWindow
        { withId = "inventory"
        , window =
            { title = "inventory"
            , closeMsg = Nothing
            }
        , rect = Rect.new ( 8, 28 ) ( 118, 225 )
        }
    , Instruction.CreateObject
        { object =
            UIObject.image
                { id = "scene:ruins"
                , url = "WindSleepers/ruins.gif"
                , size = ( 256, 172 )
                }
        }
    , Instruction.AttachObject
        { objectId = "scene:ruins"
        , parentId = "scene"
        , rect = Rect.new ( 0, 0 ) ( 256, 172 )
        }
    , Instruction.CreateObject
        { object =
            UIObject.image
                { id = "obj:skull"
                , url = "WindSleepers/skull.gif"
                , size = ( 15, 17 )
                }
        }
    , Instruction.AttachObject
        { objectId = "obj:skull"
        , parentId = "inventory"
        , rect = Rect.new ( 13, 32 ) ( 15, 17 )
        }
    ]
