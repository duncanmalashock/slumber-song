module Game exposing (Game, Msg(..), currentRoom, narration, new, objectsInCurrentRoom, objectsInInventory, player, update)

import Command exposing (Command(..))
import Effect exposing (Effect(..))
import Object exposing (Object)
import Objects exposing (Objects)


type Game
    = Game Internals


type alias Internals =
    { objects : Objects
    , selectedCommand : Maybe Command
    , sourceObject : Maybe String
    , targetObject : Maybe String
    , narration : String
    }


new : List Object -> ( Game, List Effect )
new objects =
    ( Game
        { objects = Objects.new objects
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


player : Game -> Object
player (Game internals) =
    internals.objects
        |> Objects.getById "player"
        |> Maybe.withDefault Object.null


objectsInInventory : Game -> List Object
objectsInInventory (Game internals) =
    internals.objects
        |> Objects.withParentId "player"


currentRoom : Game -> Object
currentRoom (Game internals) =
    let
        playerParentId =
            internals.objects
                |> Objects.getById "player"
                |> Maybe.withDefault Object.null
                |> Object.parent
    in
    Objects.getById playerParentId internals.objects
        |> Maybe.withDefault Object.null


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
    Objects.withParentId currentRoomId internals.objects
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
            runEngine updatedGame

        UserClickedObject objectId ->
            let
                updatedGame : Game
                updatedGame =
                    Game
                        { internals
                            | sourceObject = Just objectId
                        }
            in
            runEngine updatedGame


type Interaction
    = SelectCommand Command
    | SelectObject Object


runEngine : Game -> ( Game, List Effect )
runEngine (Game internals) =
    case toRunCommand internals of
        Just (RunExamine sourceId) ->
            let
                descriptionText : String
                descriptionText =
                    internals.objects
                        |> Objects.getById sourceId
                        |> Maybe.withDefault Object.null
                        |> Object.description
            in
            ( Game
                { internals
                    | sourceObject = Nothing
                    , selectedCommand = Nothing
                    , narration = descriptionText
                }
            , [ PrintText descriptionText
              ]
            )

        Nothing ->
            ( Game internals, [] )


type RunCommand
    = RunExamine String


toRunCommand : Internals -> Maybe RunCommand
toRunCommand internals =
    case ( internals.selectedCommand, internals.sourceObject ) of
        ( Just Examine, Just sourceObject ) ->
            Just (RunExamine sourceObject)

        _ ->
            Nothing
