module Command exposing (Command(..), decoder, fromString, listForMenu, toName, toString)

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
                case fromString str of
                    Ok command ->
                        Decode.succeed command

                    Err error ->
                        Decode.fail error
            )


fromString : String -> Result String Command
fromString string =
    case string of
        "examine" ->
            Ok Examine

        "open" ->
            Ok Open

        "close" ->
            Ok Close

        "speak" ->
            Ok Speak

        "operate" ->
            Ok Operate

        "go" ->
            Ok Go

        "hit" ->
            Ok Hit

        "consume" ->
            Ok Consume

        "move" ->
            Ok Move

        _ ->
            Err ("Unknown command: " ++ string)
