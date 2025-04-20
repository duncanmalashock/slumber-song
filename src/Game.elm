module Game exposing (Game, Msg(..), new, selectCommand, selectedCommand, update)

import Command exposing (Command(..))
import Effect exposing (Effect(..))
import Object exposing (Object)
import Objects exposing (Objects)


type Game
    = Game Internals


type alias Internals =
    { objects : Objects
    , selectedCommand : Maybe Command
    }


new : List Object -> ( Game, List Effect )
new objects =
    ( Game
        { objects = Objects.new objects
        , selectedCommand = Nothing
        }
    , []
    )


type Msg
    = UserClickedCommandButton Command


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


selectCommand : Command -> Game -> Game
selectCommand command (Game internals) =
    Game
        { internals
            | selectedCommand = Just Command.Go
        }


selectedCommand : Game -> Maybe Command
selectedCommand (Game internals) =
    internals.selectedCommand
