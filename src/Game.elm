module Game exposing (Game, Msg(..), currentRoom, narration, new, objectsInCurrentRoom, objectsInInventory, player, update)

import Command exposing (Command(..))
import Effect exposing (Effect(..))
import Object exposing (Object)
import ObjectStore exposing (ObjectStore)
import Script exposing (Script)
import Update exposing (Update(..))


type Game
    = Game Internals


type alias Internals =
    { objects : ObjectStore
    , selectedCommand : Maybe Command
    , sourceObject : Maybe String
    , targetObject : Maybe String
    , narration : String
    }


new : List Object -> ( Game, List Effect )
new objects =
    ( Game
        { objects = ObjectStore.new objects
        , selectedCommand = Nothing
        , sourceObject = Nothing
        , targetObject = Nothing
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
            runScripts updatedGame

        UserClickedObject objectId ->
            let
                updatedGame : Game
                updatedGame =
                    Game
                        { internals
                            | sourceObject = Just objectId
                        }
            in
            runScripts updatedGame


runScripts : Game -> ( Game, List Effect )
runScripts game =
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

        updatesAndEffects : List ( List Update, List Effect )
        updatesAndEffects =
            [ worldScripts
            , currentRoomScripts
            , currentRoomObjectScripts
            ]
                |> List.concat
                |> List.map Script.run

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


applyUpdate : Update -> Game -> Game
applyUpdate updateToApply (Game internals) =
    case updateToApply of
        AddToAttribute { objId, attributeKey, value } ->
            Game internals

        ClearSelections ->
            Game internals
