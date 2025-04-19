module Game exposing (Effect(..), Game, Msg(..), dummy, goToExit, new, selectCommand, selectedCommand, update)

import Command exposing (Command(..))
import Map exposing (Map)
import Room exposing (Room)


type Game
    = Game Internals


type alias Internals =
    { currentRoom : Room
    , map : Map
    , selectedCommand : Maybe Command
    }


dummy : Game
dummy =
    let
        emptyRoom : Room
        emptyRoom =
            Room.new
                { id = ""
                , name = ""
                , exits =
                    []
                }
    in
    Game
        { currentRoom = emptyRoom
        , map = Map.new [ emptyRoom ]
        , selectedCommand = Nothing
        }


new : List Room.Room -> Room.Room -> ( Game, List Effect )
new rooms initialRoom =
    ( Game
        { currentRoom = initialRoom
        , map = Map.new rooms
        , selectedCommand = Nothing
        }
    , [ UpdateRoom initialRoom ]
    )


type Msg
    = UserClickedCommandButton Command
    | UserClickedExit String


type Effect
    = UpdateRoom Room
    | PlaySound String
    | HighlightCommand Command


update : Msg -> Game -> ( Game, List Effect )
update msg ((Game internals) as game) =
    case msg of
        UserClickedCommandButton command ->
            ( Game
                { internals
                    | selectedCommand = Just command
                }
            , [ HighlightCommand command
              ]
            )

        UserClickedExit toRoomId ->
            if internals.selectedCommand == Just Command.Go then
                goToExit { toRoomId = toRoomId } game

            else
                ( game, [] )


selectCommand : Command -> Game -> Game
selectCommand command (Game internals) =
    Game
        { internals
            | selectedCommand = Just Command.Go
        }


goToExit : { toRoomId : String } -> Game -> ( Game, List Effect )
goToExit { toRoomId } (Game internals) =
    let
        newRoom : Room
        newRoom =
            internals.map
                |> Map.getRoomById toRoomId
                |> Maybe.withDefault internals.currentRoom
    in
    ( Game
        { internals
            | selectedCommand = Nothing
            , currentRoom = newRoom
        }
    , UpdateRoom newRoom
        :: (newRoom
                |> Room.soundsOnEnter
                |> List.map PlaySound
           )
    )


selectedCommand : Game -> Maybe Command
selectedCommand (Game internals) =
    internals.selectedCommand
