module Game exposing (Game, Msg(..), currentRoom, narration, new, objectsInCurrentRoom, objectsInInventory, player, update)

import Command exposing (Command(..))
import Effect exposing (Effect(..))
import Expression
import Interaction exposing (Interaction)
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
        worldScripts : List Script
        worldScripts =
            world game
                |> Object.scripts

        currentRoomScripts : List Script
        currentRoomScripts =
            currentRoom game
                |> Object.scripts

        currentRoomObjectScripts : List Script
        currentRoomObjectScripts =
            objectsInCurrentRoom game
                |> List.concatMap Object.scripts

        scriptsToRun : List Script
        scriptsToRun =
            [ worldScripts
            , currentRoomScripts
            , currentRoomObjectScripts
            ]
                |> List.concat

        updatesAndEffects : List ( List Update, List Effect )
        updatesAndEffects =
            scriptsToRun
                |> List.map (runScript interaction internals.objects)

        updates : List Update
        updates =
            updatesAndEffects
                |> List.concatMap (\( u, _ ) -> u)

        effects : List Effect
        effects =
            updatesAndEffects
                |> List.concatMap (\( _, e ) -> e)

        updatedGame : Game
        updatedGame =
            List.foldl applyUpdate game updates
    in
    ( updatedGame, effects )


runScript : Interaction -> ObjectStore -> Script -> ( List Update, List Effect )
runScript interaction objects script =
    if Trigger.shouldRun script.trigger interaction then
        if Expression.evaluate (ObjectStore.getAttribute objects) script.condition then
            ( script.updates ++ [ ClearSelections ], script.effects )

        else
            ( [ ClearSelections ], [] )

    else
        ( [ ClearSelections ], [] )


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

        ClearSelections ->
            Game
                { internals
                    | selectedCommand = Nothing
                    , sourceObjectId = Nothing
                    , targetObjectId = Nothing
                    , objectDragInfo = Nothing
                }
