module Apps.WindSleepers exposing (Model, Msg(..), init, update)

import MacOS.Instruction as Instruction exposing (Instruction)
import MacOS.Rect as Rect
import MacOS.ToAppMsg as ToAppMsg exposing (ToAppMsg(..))
import MacOS.UI.Helpers as UIHelpers exposing (domIds)
import MacOS.UI.Object as UIObject
import MacOS.UI.View as View
import MacOS.UI.View.Image as Image
import MacOS.UI.View.Textarea as Textarea
import MacOS.UI.View.Window as Window


type alias Model =
    {}


type alias ObjectId =
    String


init : ( Model, List (Instruction msg) )
init =
    ( {}
    , [ Instruction.CreateWindow
            { withId = "scene"
            , window =
                { title = "Temple Ruins"
                , closeMsg = Nothing
                }
            , rect = Rect.new ( 128, 61 ) ( 256, 192 )
            }
      , Instruction.CreateWindow
            { withId = "inventory"
            , window =
                { title = "inventory"
                , closeMsg = Nothing
                }
            , rect = Rect.new ( 3, 28 ) ( 121, 225 )
            }
      , Instruction.CreateWindow
            { withId = "narration"
            , window =
                { title = "Untitled"
                , closeMsg = Nothing
                }
            , rect = Rect.new ( 1, 255 ) ( 509, 85 )
            }
      , Instruction.CreateObject
            { object =
                UIObject.textarea
                    { id = "narration:text"
                    , font = Textarea.Chicago
                    , color = Textarea.Black
                    }
            }
      , Instruction.AttachObject
            { objectId = "narration:text"
            , parentId = "narration"
            , rect = Rect.new ( 0, 18 ) ( 496, 85 )
            }
      , Instruction.CreateObject
            { object =
                UIObject.image
                    { id = "scene:ruins"
                    , url = "WindSleepers/ruins.gif"
                    , filter = Nothing
                    , size = ( 256, 172 )
                    }
            }
      , Instruction.AttachObject
            { objectId = "scene:ruins"
            , parentId = "scene"
            , rect = Rect.new ( 0, 18 ) ( 256, 172 )
            }
      , Instruction.CreateObject
            { object =
                UIObject.image
                    { id = "obj:skull"
                    , url = "WindSleepers/skull.gif"
                    , filter = Nothing
                    , size = ( 15, 17 )
                    }
                    |> UIObject.setSelectOptions
                        { view =
                            View.image
                                { url = "WindSleepers/skull.gif"
                                , size = ( 15, 17 )
                                , filter = Just Image.Invert
                                }
                        , selected = False
                        }
                    |> UIObject.setDragOptions
                        { traveling =
                            View.image
                                { url = "WindSleepers/skull.gif"
                                , size = ( 15, 17 )
                                , filter = Nothing
                                }
                        }
            }
      , Instruction.AttachObject
            { objectId = "obj:skull"
            , parentId = "inventory"
            , rect = Rect.new ( 6, 29 ) ( 15, 17 )
            }
      ]
    )


type Msg
    = ReceivedMsgFromOS ToAppMsg


update : Msg -> Model -> ( Model, List (Instruction msg) )
update msg model =
    case msg of
        ReceivedMsgFromOS toAppMsg ->
            case toAppMsg of
                DroppedObject droppedObjectInfo ->
                    if droppedObjectInfo.isWindow then
                        ( model
                        , [ Instruction.UpdateWindowRect
                                { objectId = droppedObjectInfo.objectId
                                , rect = droppedObjectInfo.dropRectAbsolute
                                }
                          ]
                        )

                    else if droppedObjectInfo.droppedOnWindow == Just "scene" then
                        ( model
                        , [ moveDroppedObjectToWindow droppedObjectInfo
                          , unselectObject droppedObjectInfo.objectId
                          ]
                        )

                    else if droppedObjectInfo.droppedOnWindow == Just "inventory" then
                        ( model
                        , [ moveDroppedObjectToWindow droppedObjectInfo
                          , unselectObject droppedObjectInfo.objectId
                          ]
                        )

                    else if droppedObjectInfo.droppedOnWindow == Just "narration" then
                        ( model
                        , [ rejectDrop droppedObjectInfo
                          , unselectObject droppedObjectInfo.objectId
                          , print "Objects aren't allowed in the text window!"
                          ]
                        )

                    else if droppedObjectInfo.droppedOnWindow == Nothing then
                        ( model
                        , [ rejectDrop droppedObjectInfo
                          , unselectObject droppedObjectInfo.objectId
                          , print "The skull would get lost if it was put on the desktop."
                          ]
                        )

                    else
                        ( model
                        , []
                        )


moveDroppedObjectToWindow : ToAppMsg.DroppedObjectInfo -> Instruction msg
moveDroppedObjectToWindow droppedObjectInfo =
    Instruction.ReparentObjectToWindow
        { objectId = droppedObjectInfo.objectId
        , windowId = Maybe.withDefault "" droppedObjectInfo.droppedOnWindow
        , rectInWindow = droppedObjectInfo.dropRectInWindow
        }


unselectObject : ObjectId -> Instruction msg
unselectObject objectId =
    Instruction.UpdateObjectSelected
        { objectId = objectId
        , selected = False
        }


print : String -> Instruction msg
print textToPrint =
    Instruction.UpdateObjectText
        { objectId = "narration:text"
        , text = textToPrint
        }


rejectDrop : ToAppMsg.DroppedObjectInfo -> Instruction msg
rejectDrop droppedObjectInfo =
    Instruction.AnimateZoom
        { from = droppedObjectInfo.dropRectAbsolute
        , to = droppedObjectInfo.originRect
        , zoomingIn = False
        }
