module Game exposing (Game, Msg(..), currentRoom, narration, new, objectsInCurrentRoom, objectsInInventory, player, update)

import Command exposing (Command(..))
import Effect exposing (Effect(..))
import Expression
import Interaction exposing (Interaction(..))
import Object exposing (Object)
import ObjectStore exposing (ObjectStore)
import Script exposing (Script)
import Trigger exposing (Trigger)
import Update exposing (Update(..))


type Game
    = Game Internals


type alias Internals =
    { objects : ObjectStore
    , selectedCommand : Maybe Command
    , sourceObjectId : Maybe String
    , targetObjectId : Maybe String
    , objectDragInfo : Maybe { objectId : String, from : ObjectLocation, to : ObjectLocation }
    , narration : String
    }


type alias ObjectLocation =
    { windowObjectId : String
    , xPos : Int
    , yPos : Int
    }


new : List Object -> ( Game, List Effect )
new objects =
    ( Game
        { objects = ObjectStore.new objects
        , selectedCommand = Nothing
        , sourceObjectId = Nothing
        , targetObjectId = Nothing
        , objectDragInfo = Nothing
        , narration = ""
        }
    , []
    )


narration : Game -> String
narration (Game internals) =
    internals.narration


world : Game -> Object
world (Game internals) =
    internals.objects
        |> ObjectStore.getById "world"


player : Game -> Object
player (Game internals) =
    internals.objects
        |> ObjectStore.getById "player"


objectsInInventory : Game -> List Object
objectsInInventory (Game internals) =
    internals.objects
        |> ObjectStore.withParentId "player"


currentRoom : Game -> Object
currentRoom (Game internals) =
    let
        playerParentId =
            internals.objects
                |> ObjectStore.getById "player"
                |> Object.parent
    in
    ObjectStore.getById playerParentId internals.objects


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


type Msg
    = UserClickedCommandButton Command
    | UserClickedObject String


update : Msg -> Game -> ( Game, List Effect )
update msg ((Game internals) as game) =
    case msg of
        UserClickedCommandButton command ->
            let
                updatedGame : Game
                updatedGame =
                    Game
                        { internals
                            | selectedCommand = Just command
                        }
            in
            respondToInput updatedGame

        UserClickedObject objectId ->
            let
                updatedGame : Game
                updatedGame =
                    Game
                        { internals
                            | sourceObjectId = Just objectId
                        }
            in
            respondToInput updatedGame


respondToInput : Game -> ( Game, List Effect )
respondToInput ((Game internals) as game) =
    let
        maybeInteraction : Maybe Interaction
        maybeInteraction =
            Interaction.detect
                { selectedCommand = internals.selectedCommand
                , sourceObjectId = internals.sourceObjectId
                , targetObjectId = internals.targetObjectId
                , objectDragInfo = internals.objectDragInfo
                }
    in
    case maybeInteraction of
        Nothing ->
            ( Game internals, [] )

        Just interaction ->
            runScripts game interaction


runScripts : Game -> Interaction -> ( Game, List Effect )
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

        updatesAndEffects :
            List
                { handledInteraction : Bool, updates : List Update, effects : List Effect }
        updatesAndEffects =
            scriptsToRun
                |> List.map
                    (\( objId, script ) ->
                        runScript objId interaction internals.objects script
                    )
                |> finalizeUpdatesAndEffects

        finalizeUpdatesAndEffects :
            List { handledInteraction : Bool, updates : List Update, effects : List Effect }
            -> List { handledInteraction : Bool, updates : List Update, effects : List Effect }
        finalizeUpdatesAndEffects list =
            if List.any (\{ handledInteraction } -> handledInteraction) list then
                list

            else
                list
                    |> applyFallbackForInteraction interaction

        applyFallbackForInteraction :
            Interaction
            ->
                List
                    { handledInteraction : Bool, updates : List Update, effects : List Effect }
            ->
                List
                    { handledInteraction : Bool, updates : List Update, effects : List Effect }
        applyFallbackForInteraction i list =
            let
                fallback =
                    case i of
                        AttemptMoveObject { objectId } ->
                            let
                                objectName =
                                    ObjectStore.getById objectId internals.objects
                                        |> Object.name
                            in
                            { handledInteraction = True
                            , updates = []
                            , effects =
                                [ PrintText ("The " ++ objectName ++ " couldn't be moved.")
                                ]
                            }

                        AttemptExamine { objectId } ->
                            let
                                objectDescription =
                                    ObjectStore.getById objectId internals.objects
                                        |> Object.description
                            in
                            { handledInteraction = True
                            , updates = []
                            , effects =
                                [ PrintText objectDescription
                                ]
                            }

                        AttemptOpen { objectId } ->
                            let
                                objectName =
                                    ObjectStore.getById objectId internals.objects
                                        |> Object.name
                            in
                            { handledInteraction = True
                            , updates = []
                            , effects =
                                [ PrintText ("The " ++ objectName ++ " couldn't be opened.")
                                ]
                            }

                        AttemptClose { objectId } ->
                            let
                                objectName =
                                    ObjectStore.getById objectId internals.objects
                                        |> Object.name
                            in
                            { handledInteraction = True
                            , updates = []
                            , effects =
                                [ PrintText ("The " ++ objectName ++ " couldn't be closed.")
                                ]
                            }

                        AttemptSpeak { objectId } ->
                            let
                                objectName =
                                    ObjectStore.getById objectId internals.objects
                                        |> Object.name
                            in
                            { handledInteraction = True
                            , updates = []
                            , effects =
                                [ PrintText ("Speaking to the " ++ objectName ++ " produced no effect.")
                                ]
                            }

                        AttemptOperate { sourceObjectId, targetObjectId } ->
                            let
                                sourceObjectName =
                                    ObjectStore.getById sourceObjectId internals.objects
                                        |> Object.name

                                targetObjectName =
                                    ObjectStore.getById targetObjectId internals.objects
                                        |> Object.name
                            in
                            { handledInteraction = True
                            , updates = []
                            , effects =
                                [ PrintText ("Operating the " ++ sourceObjectName ++ " on the " ++ targetObjectName ++ " produced no effect.")
                                ]
                            }

                        AttemptGo { objectId } ->
                            let
                                objectName =
                                    ObjectStore.getById objectId internals.objects
                                        |> Object.name
                            in
                            { handledInteraction = True
                            , updates = []
                            , effects =
                                [ PrintText "You can't go there."
                                ]
                            }

                        AttemptHit { objectId } ->
                            let
                                objectName =
                                    ObjectStore.getById objectId internals.objects
                                        |> Object.name
                            in
                            { handledInteraction = True
                            , updates = []
                            , effects =
                                [ PrintText ("Hitting the " ++ objectName ++ " produced no effect.")
                                ]
                            }

                        AttemptConsume { objectId } ->
                            let
                                objectName =
                                    ObjectStore.getById objectId internals.objects
                                        |> Object.name
                            in
                            { handledInteraction = True
                            , updates = []
                            , effects =
                                [ PrintText ("You can't eat the " ++ objectName ++ ".")
                                ]
                            }
            in
            list ++ [ fallback ]

        finalUpdates : List Update
        finalUpdates =
            updatesAndEffects
                |> List.concatMap (\{ updates } -> updates)
                |> (++) [ ClearSelections ]

        finalEffects : List Effect
        finalEffects =
            updatesAndEffects
                |> List.concatMap (\{ effects } -> effects)

        updatedGame : Game
        updatedGame =
            List.foldl applyUpdate game finalUpdates
    in
    ( updatedGame, finalEffects )


runScript :
    String
    -> Interaction
    -> ObjectStore
    -> Script
    -> { handledInteraction : Bool, updates : List Update, effects : List Effect }
runScript objectId interaction objects script =
    if
        Trigger.shouldRun objectId script.trigger interaction
            && Expression.evaluate (ObjectStore.getAttribute objects) script.condition
    then
        { handledInteraction = Interaction.handlesObject objectId interaction
        , updates = script.updates
        , effects = script.effects
        }

    else
        { handledInteraction = False
        , updates = []
        , effects = []
        }


applyUpdate : Update -> Game -> Game
applyUpdate updateToApply (Game internals) =
    case updateToApply of
        IncrementAttribute { objId, attributeKey, value } ->
            Game
                { internals
                    | objects =
                        ObjectStore.incrementAttributeBy
                            internals.objects
                            { objectId = objId
                            , attributeId = attributeKey
                            , amount = value
                            }
                }

        SetBoolAttribute { objId, attributeKey, value } ->
            Game
                { internals
                    | objects =
                        ObjectStore.setBoolAttribute
                            internals.objects
                            { objectId = objId
                            , attributeId = attributeKey
                            , value = value
                            }
                }

        ClearSelections ->
            Game
                { internals
                    | selectedCommand = Nothing
                    , sourceObjectId = Nothing
                    , targetObjectId = Nothing
                    , objectDragInfo = Nothing
                }
