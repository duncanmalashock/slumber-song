module Command exposing (Command(..), toString)


type Command
    = Go


toString : Command -> String
toString command =
    case command of
        Go ->
            "go"
