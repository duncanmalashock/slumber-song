module Vent.Game exposing (Game, currentRoom, decoder, new, objects, objectsInCurrentRoom, objectsInInventory, player, update)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra
import MacOS.Coordinate as Coordinate
import MacOS.Instruction as Instruction exposing (Instruction)
import MacOS.Rect as Rect exposing (Rect)
import MacOS.ToAppMsg as ToAppMsg exposing (ToAppMsg(..))
import MacOS.UI.Object as UIObject
import MacOS.UI.View as View
import MacOS.UI.View.Image as Image
import MacOS.UI.View.Textarea as Textarea
import Random
import Vent.Command exposing (Command(..))
import Vent.Effect as Effect exposing (Effect(..))
import Vent.Interaction as Interaction exposing (Interaction(..))
import Vent.Object as Object exposing (Object)
import Vent.ObjectStore as ObjectStore exposing (ObjectStore)
import Vent.VentScript.Expression as Expression
import Vent.VentScript.Script exposing (Script)
import Vent.VentScript.Statement as Statement exposing (Statement(..))
import Vent.VentScript.Trigger as Trigger exposing (Trigger)


type Game
    = Game Internals


type alias Internals =
    { responses : Responses
    , objects : ObjectStore
    , randomSeed : Random.Seed
    , selectedCommand : Maybe Command
    , sourceObjectId : Maybe String
    , targetObjectId : Maybe String
    }


type alias Responses =
    { immovableObject : List String
    , illegalDrop : IllegalDropResponses
    }


type alias IllegalDropResponses =
    { onDesktop : String
    , onTextWindow : String
    }


type alias ObjectLocation =
    { windowObjectId : String
    , xPos : Int
    , yPos : Int
    }


new : Game -> ( Game, List (Instruction msg) )
new ((Game internals) as game) =
    let
        roomContainingPlayer : Object
        roomContainingPlayer =
            currentRoom game

        createCurrentRoom : List (Instruction msg)
        createCurrentRoom =
            List.concat
                [ createRoom
                    { id = Object.id roomContainingPlayer
                    , image = Object.image roomContainingPlayer
                    }
                , List.concat
                    (List.map createRoomObject (ObjectStore.withParentId (Object.id roomContainingPlayer) internals.objects))
                ]

        createRoomObject : Object -> List (Instruction msg)
        createRoomObject object =
            createSceneObject
                { id = Object.id object
                , image = Object.image object
                , rect = Object.rect object
                }
    in
    ( game
    , List.concat
        [ createSceneWindow
        , createInventoryWindow
        , createNarrationWindow
        , createCurrentRoom
        , createInventoryObject
            { id = "obj:torch"
            , image = "torch"
            , rect = Rect.new ( 8, 24 ) ( 35, 92 )
            }
        ]
    )


update : ToAppMsg -> Game -> ( Game, List (Instruction msg) )
update toAppMsg ((Game internals) as game) =
    case toAppMsg of
        DroppedUIObject droppedObjectInfo ->
            if droppedObjectInfo.isWindow then
                ( game
                , [ Instruction.UpdateWindowRect
                        { objectId = droppedObjectInfo.objectId
                        , rect = droppedObjectInfo.dropRectAbsolute
                        }
                  ]
                )

            else if droppedObjectInfo.droppedOnWindow == Just windowIds.narration then
                ( game
                , [ rejectDrop droppedObjectInfo
                  , unselectObject droppedObjectInfo.objectId
                  , print internals.responses.illegalDrop.onTextWindow
                  ]
                )

            else if droppedObjectInfo.droppedOnWindow == Nothing then
                let
                    objectName : Maybe String
                    objectName =
                        ObjectStore.get droppedObjectInfo.objectId internals.objects
                            |> Maybe.map Object.name
                in
                ( game
                , List.filterMap identity
                    [ Just (rejectDrop droppedObjectInfo)
                    , Just (unselectObject droppedObjectInfo.objectId)
                    , Maybe.map
                        (\name ->
                            internals.responses.illegalDrop.onDesktop
                                |> interpolateObjName name
                                |> print
                        )
                        objectName
                    ]
                )

            else if ObjectStore.isImmovable droppedObjectInfo.objectId internals.objects then
                let
                    ( responseIndex, newRandomSeed ) =
                        Random.step
                            (Random.int 0 (List.length internals.responses.immovableObject - 1))
                            internals.randomSeed

                    response : String
                    response =
                        List.Extra.getAt responseIndex internals.responses.immovableObject
                            |> Maybe.withDefault ""

                    objectName : Maybe String
                    objectName =
                        ObjectStore.get droppedObjectInfo.objectId internals.objects
                            |> Maybe.map Object.name
                in
                ( Game
                    { internals
                        | randomSeed = newRandomSeed
                    }
                , List.filterMap identity
                    [ Just (rejectDrop droppedObjectInfo)
                    , Just (unselectObject droppedObjectInfo.objectId)
                    , Maybe.map
                        (\name ->
                            interpolateObjName name response
                                |> print
                        )
                        objectName
                    ]
                )

            else if droppedObjectInfo.droppedOnWindow == Just windowIds.scene then
                ( game
                , [ moveDroppedObjectToWindow droppedObjectInfo
                  , unselectObject droppedObjectInfo.objectId
                  ]
                )

            else if droppedObjectInfo.droppedOnWindow == Just windowIds.inventory then
                ( game
                , [ moveDroppedObjectToWindow droppedObjectInfo
                  , unselectObject droppedObjectInfo.objectId
                  ]
                )

            else
                ( game
                , []
                )

        DoubleClickedUIObject doubleClickedObjectInfo ->
            let
                descriptionText : Maybe String
                descriptionText =
                    ObjectStore.get doubleClickedObjectInfo.objectId internals.objects
                        |> Maybe.map Object.description
            in
            ( game
            , List.filterMap identity
                [ Just (unselectObject doubleClickedObjectInfo.objectId)
                , Maybe.map print descriptionText
                ]
            )


interpolateObjName : String -> String -> String
interpolateObjName replaceWith containingKeyword =
    String.replace "$obj" replaceWith containingKeyword


moveDroppedObjectToWindow : ToAppMsg.DroppedUIObjectInfo -> Instruction msg
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


rejectDrop : ToAppMsg.DroppedUIObjectInfo -> Instruction msg
rejectDrop droppedObjectInfo =
    Instruction.AnimateZoom
        { from = droppedObjectInfo.dropRectAbsolute
        , to = droppedObjectInfo.originRect
        , zoomingIn = False
        }


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
                , size = Coordinate.new ( 256, 171 )
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
    , rect : Rect
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
                , size = Rect.size params.rect
                }
                |> UIObject.setSelectOptions
                    { view =
                        View.image
                            { url = imageUrl
                            , size = Rect.size params.rect
                            , filter = Just Image.Invert
                            }
                    , selected = False
                    }
                |> UIObject.setDragOptions
                    { traveling =
                        View.image
                            { url = imageUrl
                            , size = Rect.size params.rect
                            , filter = Nothing
                            }
                    , preDragInPixels = objectPreDrag
                    }
        }
    , Instruction.AttachObject
        { objectId = params.id
        , parentId = params.parentId
        , rect = params.rect
        }
    ]


createSceneObject :
    { id : String
    , image : String
    , rect : Rect
    }
    -> List (Instruction msg)
createSceneObject params =
    createObject
        { id = params.id
        , image = params.image
        , rect = params.rect
        , parentId = windowIds.scene
        }


createInventoryObject :
    { id : String
    , image : String
    , rect : Rect
    }
    -> List (Instruction msg)
createInventoryObject params =
    createObject
        { id = params.id
        , image = params.image
        , rect = params.rect
        , parentId = windowIds.inventory
        }


objects : Game -> ObjectStore
objects (Game internals) =
    internals.objects


world : Game -> Object
world (Game internals) =
    internals.objects
        |> ObjectStore.getNoFail "world"


player : Game -> Object
player (Game internals) =
    internals.objects
        |> ObjectStore.getNoFail "player"


objectsInInventory : Game -> List Object
objectsInInventory (Game internals) =
    internals.objects
        |> ObjectStore.withParentId "player"


currentRoom : Game -> Object
currentRoom (Game internals) =
    let
        playerParentId =
            internals.objects
                |> ObjectStore.getNoFail "player"
                |> Object.parent
    in
    ObjectStore.getNoFail playerParentId internals.objects


objectsInCurrentRoom : Game -> List Object
objectsInCurrentRoom (Game internals) =
    let
        currentRoomId =
            currentRoom (Game internals)
                |> Object.id

        excludePlayer : List Object -> List Object
        excludePlayer objectsList =
            objectsList
                |> List.filter (\obj -> Object.id obj /= "player")
    in
    ObjectStore.withParentId currentRoomId internals.objects
        |> excludePlayer


respondToInput : Game -> ( Game, List (Instruction msg) )
respondToInput ((Game internals) as game) =
    let
        maybeInteraction : Maybe Interaction
        maybeInteraction =
            Interaction.detect
                { selectedCommand = internals.selectedCommand
                , sourceObjectId = internals.sourceObjectId
                , targetObjectId = internals.targetObjectId
                }
    in
    case maybeInteraction of
        Nothing ->
            ( Game internals, [] )

        Just interaction ->
            runScripts game interaction


runScripts : Game -> Interaction -> ( Game, List (Instruction msg) )
runScripts ((Game internals) as game) interaction =
    let
        objectIdAndScripts : Object -> List ( String, Script )
        objectIdAndScripts obj =
            obj
                |> Object.scripts
                |> List.map
                    (\s ->
                        ( Object.id obj, s )
                    )

        worldScripts : List ( String, Script )
        worldScripts =
            world game
                |> objectIdAndScripts

        currentRoomScripts : List ( String, Script )
        currentRoomScripts =
            currentRoom game
                |> objectIdAndScripts

        currentRoomObjectScripts : List ( String, Script )
        currentRoomObjectScripts =
            objectsInCurrentRoom game
                |> List.concatMap objectIdAndScripts

        scriptsToRun : List ( String, Script )
        scriptsToRun =
            [ worldScripts
            , currentRoomScripts
            , currentRoomObjectScripts
            ]
                |> List.concat

        statements :
            List
                { handledInteraction : Bool, statements : List Statement }
        statements =
            scriptsToRun
                |> List.map
                    (\( objId, script ) ->
                        runScript objId interaction internals.objects script
                    )
                |> finalizeStatements

        finalizeStatements :
            List { handledInteraction : Bool, statements : List Statement }
            -> List { handledInteraction : Bool, statements : List Statement }
        finalizeStatements list =
            if List.any (\{ handledInteraction } -> handledInteraction) list then
                list

            else
                list
                    |> applyFallbackForInteraction interaction

        applyFallbackForInteraction :
            Interaction
            ->
                List
                    { handledInteraction : Bool, statements : List Statement }
            ->
                List
                    { handledInteraction : Bool, statements : List Statement }
        applyFallbackForInteraction i list =
            let
                fallback : { handledInteraction : Bool, statements : List Statement }
                fallback =
                    case i of
                        AttemptMoveObject { objectId } ->
                            let
                                objectName =
                                    ObjectStore.getNoFail objectId internals.objects
                                        |> Object.name
                            in
                            { handledInteraction = True
                            , statements =
                                [ Statement.PrintText ("The " ++ objectName ++ " couldn't be moved.")
                                ]
                            }

                        AttemptExamine { objectId } ->
                            let
                                objectDescription =
                                    ObjectStore.getNoFail objectId internals.objects
                                        |> Object.description
                            in
                            { handledInteraction = True
                            , statements =
                                [ Statement.PrintText objectDescription
                                ]
                            }

                        AttemptOpen { objectId } ->
                            let
                                objectName =
                                    ObjectStore.getNoFail objectId internals.objects
                                        |> Object.name
                            in
                            { handledInteraction = True
                            , statements =
                                [ Statement.PrintText ("The " ++ objectName ++ " couldn't be opened.")
                                ]
                            }

                        AttemptClose { objectId } ->
                            let
                                objectName =
                                    ObjectStore.getNoFail objectId internals.objects
                                        |> Object.name
                            in
                            { handledInteraction = True
                            , statements =
                                [ Statement.PrintText ("The " ++ objectName ++ " couldn't be closed.")
                                ]
                            }

                        AttemptSpeak { objectId } ->
                            let
                                objectName =
                                    ObjectStore.getNoFail objectId internals.objects
                                        |> Object.name
                            in
                            { handledInteraction = True
                            , statements =
                                [ Statement.PrintText ("Speaking to the " ++ objectName ++ " produced no effect.")
                                ]
                            }

                        AttemptOperate { sourceObjectId, targetObjectId } ->
                            let
                                sourceObjectName =
                                    ObjectStore.getNoFail sourceObjectId internals.objects
                                        |> Object.name

                                targetObjectName =
                                    ObjectStore.getNoFail targetObjectId internals.objects
                                        |> Object.name
                            in
                            { handledInteraction = True
                            , statements =
                                [ Statement.PrintText ("Operating the " ++ sourceObjectName ++ " on the " ++ targetObjectName ++ " produced no effect.")
                                ]
                            }

                        AttemptGo { objectId } ->
                            let
                                objectName =
                                    ObjectStore.getNoFail objectId internals.objects
                                        |> Object.name
                            in
                            { handledInteraction = True
                            , statements =
                                [ Statement.PrintText "You can't go there."
                                ]
                            }

                        AttemptHit { objectId } ->
                            let
                                objectName =
                                    ObjectStore.getNoFail objectId internals.objects
                                        |> Object.name
                            in
                            { handledInteraction = True
                            , statements =
                                [ Statement.PrintText ("Hitting the " ++ objectName ++ " accomplished nothing.")
                                ]
                            }

                        AttemptConsume { objectId } ->
                            let
                                objectName =
                                    ObjectStore.getNoFail objectId internals.objects
                                        |> Object.name
                            in
                            { handledInteraction = True
                            , statements =
                                [ Statement.PrintText ("You can't eat the " ++ objectName ++ ".")
                                ]
                            }
            in
            list ++ [ fallback ]

        updatedGame : ( Game, List (Instruction msg) )
        updatedGame =
            List.foldl applyStatement ( game, [] ) (List.concatMap .statements statements)
    in
    updatedGame


runScript :
    String
    -> Interaction
    -> ObjectStore
    -> Script
    -> { handledInteraction : Bool, statements : List Statement }
runScript objectId interaction objectStore script =
    if
        Trigger.shouldRun objectId script.trigger interaction
            && Expression.evaluate (ObjectStore.getAttribute objectStore) script.condition
    then
        { handledInteraction = Interaction.handlesObject objectId interaction
        , statements = script.statements
        }

    else
        { handledInteraction = False
        , statements = []
        }


applyStatement : Statement -> ( Game, List (Instruction msg) ) -> ( Game, List (Instruction msg) )
applyStatement statementToApply ( (Game internals) as game, effects ) =
    case statementToApply of
        IncrementAttribute { objId, attributeKey, value } ->
            ( Game
                { internals
                    | objects =
                        ObjectStore.incrementAttributeBy
                            internals.objects
                            { objectId = objId
                            , attributeId = attributeKey
                            , amount = value
                            }
                }
            , effects
            )

        SetBoolAttribute { objId, attributeKey, value } ->
            ( Game
                { internals
                    | objects =
                        ObjectStore.setBoolAttribute
                            internals.objects
                            { objectId = objId
                            , attributeId = attributeKey
                            , value = Expression.evaluate (ObjectStore.getAttribute internals.objects) value
                            }
                }
            , effects
            )

        SetIntAttribute { objId, attributeKey, value } ->
            ( Game
                { internals
                    | objects =
                        ObjectStore.setIntAttribute
                            internals.objects
                            { objectId = objId
                            , attributeId = attributeKey
                            , value = Expression.evaluateInt (ObjectStore.getAttribute internals.objects) value
                            }
                }
            , effects
            )

        SetStringAttribute { objId, attributeKey, value } ->
            ( Game
                { internals
                    | objects =
                        ObjectStore.setStringAttribute
                            internals.objects
                            { objectId = objId
                            , attributeId = attributeKey
                            , value = Expression.evaluateString (ObjectStore.getAttribute internals.objects) value
                            }
                }
            , effects
            )

        ClearSelections ->
            ( Game
                { internals
                    | selectedCommand = Nothing
                    , sourceObjectId = Nothing
                    , targetObjectId = Nothing
                }
            , effects
            )

        LoadGameData filename ->
            ( game
            , []
            )

        PlaySound filename ->
            ( game
            , []
            )

        Statement.PrintText text ->
            ( game
            , []
            )


responsesDecoder : Decoder Responses
responsesDecoder =
    Decode.map2 Responses
        (Decode.field "immovableObject" (Decode.list Decode.string))
        (Decode.field "illegalDrop" illegalDropDecoder)


illegalDropDecoder : Decoder IllegalDropResponses
illegalDropDecoder =
    Decode.map2 IllegalDropResponses
        (Decode.field "onDesktop" Decode.string)
        (Decode.field "onTextWindow" Decode.string)


decoder : Int -> Decoder Game
decoder initialSeed =
    let
        constructGame : Responses -> List Object -> Game
        constructGame myResponses myObjects =
            Game
                { responses = myResponses
                , objects = ObjectStore.new myObjects
                , randomSeed = Random.initialSeed initialSeed
                , selectedCommand = Nothing
                , sourceObjectId = Nothing
                , targetObjectId = Nothing
                }
    in
    Decode.map2 constructGame
        (Decode.field "responses" responsesDecoder)
        (Decode.field "objects" (Decode.list Object.decoder))
