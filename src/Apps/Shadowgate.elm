module Apps.Shadowgate exposing (Model, Msg(..), init, objectIds, update)

import MacOS.Instruction as Instruction exposing (Instruction)
import MacOS.Rect as Rect
import MacOS.ToAppMsg as ToAppMsg exposing (ToAppMsg(..))
import MacOS.UI.Helpers as UIHelpers exposing (domIds)
import MacOS.UI.Object as UIObject
import MacOS.UI.View as View
import MacOS.UI.View.Image as Image
import MacOS.UI.View.Textarea as Textarea
import MacOS.UI.View.Window as Window
import Vent.GameFile as GameFile exposing (GameFile)


type alias Model =
    {}


type alias ObjectId =
    String


windowIds =
    { scene = "window:scene"
    , inventory = "window:inventory"
    , narration = "window:narration"
    }


objectIds =
    { narrationText = "narration:text"
    }


createSceneWindow : List (Instruction msg)
createSceneWindow =
    [ Instruction.CreateWindow
        { withId = windowIds.scene
        , window =
            { title = "Entrance"
            , closeMsg = Nothing
            }
        , rect = Rect.new ( 128, 61 ) ( 256, 191 )
        }
    ]


createInventoryWindow : List (Instruction msg)
createInventoryWindow =
    [ Instruction.CreateWindow
        { withId = windowIds.inventory
        , window =
            { title = "inventory"
            , closeMsg = Nothing
            }
        , rect = Rect.new ( 3, 28 ) ( 121, 225 )
        }
    ]


createNarrationWindow : List (Instruction msg)
createNarrationWindow =
    [ Instruction.CreateWindow
        { withId = windowIds.narration
        , window =
            { title = "Untitled"
            , closeMsg = Nothing
            }
        , rect = Rect.new ( 1, 255 ) ( 509, 85 )
        }
    , Instruction.CreateObject
        { object =
            UIObject.textarea
                { id = objectIds.narrationText
                , font = Textarea.Chicago
                , color = Textarea.Black
                }
        }
    , Instruction.AttachObject
        { objectId = objectIds.narrationText
        , parentId = windowIds.narration
        , rect = Rect.new ( 0, 18 ) ( 496, 85 )
        }
    ]


createRoom :
    { id : String
    , image : String
    }
    -> List (Instruction msg)
createRoom params =
    let
        imageUrl : String
        imageUrl =
            "Shadowgate/" ++ params.image ++ ".gif"
    in
    [ Instruction.CreateObject
        { object =
            UIObject.image
                { id = params.id
                , url = imageUrl
                , filter = Nothing
                , size = ( 256, 171 )
                }
        }
    , Instruction.AttachObject
        { objectId = params.id
        , parentId = windowIds.scene
        , rect = Rect.new ( 0, 18 ) ( 256, 171 )
        }
    ]


createObject :
    { id : String
    , image : String
    , size : ( Int, Int )
    , position : ( Int, Int )
    , parentId : String
    }
    -> List (Instruction msg)
createObject params =
    let
        objectPreDrag : Int
        objectPreDrag =
            6

        imageUrl : String
        imageUrl =
            "Shadowgate/" ++ params.image ++ ".gif"
    in
    [ Instruction.CreateObject
        { object =
            UIObject.image
                { id = params.id
                , url = imageUrl
                , filter = Nothing
                , size = params.size
                }
                |> UIObject.setSelectOptions
                    { view =
                        View.image
                            { url = imageUrl
                            , size = params.size
                            , filter = Just Image.Invert
                            }
                    , selected = False
                    }
                |> UIObject.setDragOptions
                    { traveling =
                        View.image
                            { url = imageUrl
                            , size = params.size
                            , filter = Nothing
                            }
                    , preDragInPixels = objectPreDrag
                    }
        }
    , Instruction.AttachObject
        { objectId = params.id
        , parentId = params.parentId
        , rect = Rect.new params.position params.size
        }
    ]


createSceneObject :
    { id : String
    , image : String
    , size : ( Int, Int )
    , position : ( Int, Int )
    }
    -> List (Instruction msg)
createSceneObject params =
    createObject
        { id = params.id
        , image = params.image
        , size = params.size
        , position = params.position
        , parentId = windowIds.scene
        }


createInventoryObject :
    { id : String
    , image : String
    , size : ( Int, Int )
    , position : ( Int, Int )
    }
    -> List (Instruction msg)
createInventoryObject params =
    createObject
        { id = params.id
        , image = params.image
        , size = params.size
        , position = params.position
        , parentId = windowIds.inventory
        }


init : GameFile -> ( Model, List (Instruction msg) )
init gameFile =
    ( {}
    , List.concat
        [ createSceneWindow
        , createInventoryWindow
        , createNarrationWindow
        , createRoom
            { id = "room:entrance"
            , image = "entrance"
            }
        , createSceneObject
            { id = "obj:entrance-door"
            , image = "entrance-door"
            , size = ( 63, 122 )
            , position = ( 96, 33 )
            }
        , createSceneObject
            { id = "obj:entrance-key"
            , image = "entrance-key"
            , size = ( 15, 8 )
            , position = ( 120, 25 )
            }
        , createSceneObject
            { id = "obj:entrance-skull"
            , image = "entrance-skull"
            , size = ( 25, 18 )
            , position = ( 116, 19 )
            }
        , createInventoryObject
            { id = "obj:torch"
            , image = "torch"
            , size = ( 35, 92 )
            , position = ( 8, 24 )
            }
        , [ print "of the Titans, from the depths of the earth.  You are the seed of prophecy, the last of the line of kings, and only you can stop the Warlock Lord from darkening our world FOREVER.  Fare thee well.\"" ]
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

                    else if droppedObjectInfo.droppedOnWindow == Just windowIds.scene then
                        ( model
                        , [ moveDroppedObjectToWindow droppedObjectInfo
                          , unselectObject droppedObjectInfo.objectId
                          ]
                        )

                    else if droppedObjectInfo.droppedOnWindow == Just windowIds.inventory then
                        ( model
                        , [ moveDroppedObjectToWindow droppedObjectInfo
                          , unselectObject droppedObjectInfo.objectId
                          ]
                        )

                    else if droppedObjectInfo.droppedOnWindow == Just windowIds.narration then
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

                                "obj:entrance-skull" ->
                                    Just "It's the skull of some creature.  Whatever it is, its meaning is quite clear: Death lurks inside."

                                "obj:entrance-door" ->
                                    Just "It's a heavy wooden door."

                                "obj:entrance-key" ->
                                    Just "It's a small, brass key."

                                "room:entrance" ->
                                    Just "You stand before a stone wall that has been carved out of the earth.  The forest ends some twenty feet from the wall, as if sensing some great evil."

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
        { objectId = objectIds.narrationText
        , text = textToPrint
        }


rejectDrop : ToAppMsg.DroppedObjectInfo -> Instruction msg
rejectDrop droppedObjectInfo =
    Instruction.AnimateZoom
        { from = droppedObjectInfo.dropRectAbsolute
        , to = droppedObjectInfo.originRect
        , zoomingIn = False
        }
