module Command exposing (Command(..), decoder, listForMenu, toName, toString)

import Json.Decode as Decode exposing (Decoder)


type Command
    = Examine
    | Open
    | Close
    | Speak
    | Operate
    | Go
    | Hit
    | Consume
    | Move


listForMenu : List Command
listForMenu =
    [ Examine
    , Open
    , Close
    , Speak
    , Operate
    , Go
    , Hit
    , Consume
    ]


toString : Command -> String
toString command =
    case command of
        Examine ->
            "examine"

        Open ->
            "open"

        Close ->
            "close"

        Speak ->
            "speak"

        Operate ->
            "operate"

        Go ->
            "go"

        Hit ->
            "hit"

        Consume ->
            "consume"

        Move ->
            "move"


toName : Command -> String
toName command =
    case command of
        Examine ->
            "Examine"

        Open ->
            "Open"

        Close ->
            "Close"

        Speak ->
            "Speak"

        Operate ->
            "Operate"

        Go ->
            "Go"

        Hit ->
            "Hit"

        Consume ->
            "Consume"

        Move ->
            "Move"


decoder : Decoder Command
decoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "examine" ->
                        Decode.succeed Examine

                    "open" ->
                        Decode.succeed Open

                    "close" ->
                        Decode.succeed Close

                    "speak" ->
                        Decode.succeed Speak

                    "operate" ->
                        Decode.succeed Operate

                    "go" ->
                        Decode.succeed Go

                    "hit" ->
                        Decode.succeed Hit

                    "consume" ->
                        Decode.succeed Consume

                    "move" ->
                        Decode.succeed Move

                    _ ->
                        Decode.fail ("Unknown command: " ++ str)
            )
