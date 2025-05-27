module Apps.Shadowgate exposing (Model, Msg(..), init, update)

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
    let
        objectPreDrag : Int
        objectPreDrag =
            6
    in
    ( {}
    , [ Instruction.CreateWindow
            { withId = "scene"
            , window =
                { title = "Entrance"
                , closeMsg = Nothing
                }
            , rect = Rect.new ( 128, 61 ) ( 256, 191 )
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
                    { id = "scene:entrance"
                    , url = "Shadowgate/entrance.gif"
                    , filter = Nothing
                    , size = ( 256, 171 )
                    }
            }
      , Instruction.AttachObject
            { objectId = "scene:entrance"
            , parentId = "scene"
            , rect = Rect.new ( 0, 18 ) ( 256, 171 )
            }
      , Instruction.CreateObject
            { object =
                UIObject.image
                    { id = "obj:entrance-door"
                    , url = "Shadowgate/entrance-door.gif"
                    , filter = Nothing
                    , size = ( 63, 122 )
                    }
                    |> UIObject.setSelectOptions
                        { view =
                            View.image
                                { url = "Shadowgate/entrance-door.gif"
                                , size = ( 63, 122 )
                                , filter = Just Image.Invert
                                }
                        , selected = False
                        }
                    |> UIObject.setDragOptions
                        { traveling =
                            View.image
                                { url = "Shadowgate/entrance-door.gif"
                                , size = ( 63, 122 )
                                , filter = Nothing
                                }
                        , preDragInPixels = objectPreDrag
                        }
            }
      , Instruction.AttachObject
            { objectId = "obj:entrance-door"
            , parentId = "scene"
            , rect = Rect.new ( 96, 33 ) ( 63, 122 )
            }
      , Instruction.CreateObject
            { object =
                UIObject.image
                    { id = "obj:entrance-key"
                    , url = "Shadowgate/entrance-key.gif"
                    , filter = Nothing
                    , size = ( 15, 8 )
                    }
                    |> UIObject.setSelectOptions
                        { view =
                            View.image
                                { url = "Shadowgate/entrance-key.gif"
                                , size = ( 15, 8 )
                                , filter = Just Image.Invert
                                }
                        , selected = False
                        }
                    |> UIObject.setDragOptions
                        { traveling =
                            View.image
                                { url = "Shadowgate/entrance-key.gif"
                                , size = ( 15, 8 )
                                , filter = Nothing
                                }
                        , preDragInPixels = objectPreDrag
                        }
            }
      , Instruction.AttachObject
            { objectId = "obj:entrance-key"
            , parentId = "scene"
            , rect = Rect.new ( 120, 25 ) ( 16, 8 )
            }
      , Instruction.CreateObject
            { object =
                UIObject.image
                    { id = "obj:skull"
                    , url = "Shadowgate/door-skull.gif"
                    , filter = Nothing
                    , size = ( 25, 18 )
                    }
                    |> UIObject.setSelectOptions
                        { view =
                            View.image
                                { url = "Shadowgate/door-skull.gif"
                                , size = ( 25, 18 )
                                , filter = Just Image.Invert
                                }
                        , selected = False
                        }
                    |> UIObject.setDragOptions
                        { traveling =
                            View.image
                                { url = "Shadowgate/door-skull.gif"
                                , size = ( 25, 18 )
                                , filter = Nothing
                                }
                        , preDragInPixels = objectPreDrag
                        }
            }
      , Instruction.AttachObject
            { objectId = "obj:skull"
            , parentId = "scene"
            , rect = Rect.new ( 116, 19 ) ( 25, 18 )
            }
      , Instruction.CreateObject
            { object =
                UIObject.image
                    { id = "obj:torch"
                    , url = "Shadowgate/torch.gif"
                    , filter = Nothing
                    , size = ( 35, 92 )
                    }
                    |> UIObject.setSelectOptions
                        { view =
                            View.image
                                { url = "Shadowgate/torch.gif"
                                , size = ( 35, 92 )
                                , filter = Just Image.Invert
                                }
                        , selected = False
                        }
                    |> UIObject.setDragOptions
                        { traveling =
                            View.image
                                { url = "Shadowgate/torch.gif"
                                , size = ( 35, 92 )
                                , filter = Nothing
                                }
                        , preDragInPixels = objectPreDrag
                        }
            }
      , Instruction.AttachObject
            { objectId = "obj:torch"
            , parentId = "inventory"
            , rect = Rect.new ( 8, 24 ) ( 35, 92 )
            }
      , print "Good evening. Welcome to Shadowgate."
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

                DoubleClickedObject doubleClickedObjectInfo ->
                    let
                        descriptionText : Maybe String
                        descriptionText =
                            case doubleClickedObjectInfo.objectId of
                                "obj:torch" ->
                                    Just "It's a common, wooden torch."

                                "obj:skull" ->
                                    Just "It's the skull of some creature. Whatever it is, its meaning is quite clear: Death lurks inside."

                                "obj:entrance-door" ->
                                    Just "It's a heavy wooden door."

                                "obj:entrance-key" ->
                                    Just "It's a small, brass key."

                                _ ->
                                    Nothing
                    in
                    ( model
                    , List.filterMap identity
                        [ Just (unselectObject doubleClickedObjectInfo.objectId)
                        , Maybe.map print descriptionText
                        ]
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
