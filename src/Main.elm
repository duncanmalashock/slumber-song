module Main exposing (main)

import Browser
import Command exposing (Command)
import Dict exposing (Dict)
import Effect exposing (Effect(..))
import Game exposing (Game)
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Object exposing (Object)
import Parser
import Ports
import Vent


type alias Flags =
    { useVDomInterface : Bool
    , gameFile : String
    }


flagsDecoder : Decode.Decoder Flags
flagsDecoder =
    Decode.map2 Flags
        (Decode.field "useVDomInterface" Decode.bool)
        (Decode.field "gameFile" Decode.string)


type alias Model =
    { game : RemoteData Game.Game String
    , interfaceMode : InterfaceMode
    , parserInput : String
    }


type RemoteData data err
    = NotLoaded
    | Loading
    | LoadSuccessful data
    | LoadFailed err


type InterfaceMode
    = InterfaceJS
    | InterfaceElmVDom


type Msg
    = GameDataLoaded (List Object)
    | ReceivedGameMsg Game.Msg
    | GameMsgDecodeError Decode.Error
    | UserClickedSaveButton
    | UserTypedIntoParserInput String


msgDecoder : Decode.Decoder Msg
msgDecoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "GameDataLoaded" ->
                        Decode.field "payload"
                            (Decode.map GameDataLoaded
                                (Decode.list Object.decoder)
                            )

                    "UserClickedCommandButton" ->
                        Decode.field "payload"
                            (Decode.map
                                (Game.UserClickedCommandButton
                                    >> ReceivedGameMsg
                                )
                                (Decode.field "command" Command.decoder)
                            )

                    _ ->
                        Decode.fail "Unknown tag"
            )


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialModel : Model
        initialModel =
            { game = Loading
            , interfaceMode =
                if flags.useVDomInterface then
                    InterfaceElmVDom

                else
                    InterfaceJS
            , parserInput = String.trimLeft """
%open
if !@skull.isOpen then
@skull.isOpen = true
$printText "As if by magic, the skull rises."
end
"""
            }
    in
    ( initialModel
    , Ports.send
        [ Effect.LoadGameData flags.gameFile
        ]
    )


updateLoadedGame :
    Game.Msg
    -> RemoteData Game.Game String
    -> ( RemoteData Game.Game String, List Effect.Effect )
updateLoadedGame gameMsg gameData =
    case gameData of
        NotLoaded ->
            ( gameData, [] )

        Loading ->
            ( gameData, [] )

        LoadSuccessful loadedGame ->
            let
                ( updatedGame, effects ) =
                    Game.update gameMsg loadedGame
            in
            ( LoadSuccessful updatedGame, effects )

        LoadFailed err ->
            ( gameData
            , [ Effect.ReportError "Couldn't load game" ]
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameDataLoaded objects ->
            let
                ( newGame, effects ) =
                    Game.new objects
            in
            ( { model
                | game = LoadSuccessful newGame
              }
            , Ports.send effects
            )

        ReceivedGameMsg gameMsg ->
            let
                ( updatedGame, effects ) =
                    updateLoadedGame gameMsg model.game
            in
            ( { model
                | game = updatedGame
              }
            , Ports.send effects
            )

        GameMsgDecodeError decodeError ->
            ( model
            , Ports.send
                [ Effect.ReportError <|
                    "Couldn't decode msg: "
                        ++ Decode.errorToString decodeError
                ]
            )

        UserClickedSaveButton ->
            case model.game of
                NotLoaded ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                LoadSuccessful loadedGame ->
                    ( model
                    , Ports.send [ SaveGameData (Game.encode loadedGame) ]
                    )

                LoadFailed err ->
                    ( model, Cmd.none )

        UserTypedIntoParserInput input ->
            ( { model | parserInput = input }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receive jsonToMsg


jsonToMsg : Decode.Value -> Msg
jsonToMsg json =
    case Decode.decodeValue msgDecoder json of
        Ok msg ->
            msg

        Err err ->
            GameMsgDecodeError err


view : Model -> Html Msg
view model =
    case model.interfaceMode of
        InterfaceJS ->
            Html.text ""

        InterfaceElmVDom ->
            elmView model


elmView : Model -> Html Msg
elmView model =
    case model.game of
        LoadSuccessful game ->
            Html.div []
                [ viewCommands game
                , viewObjects game
                , viewNarration game
                , viewSaveButton game
                , viewParserInput model.parserInput
                ]

        NotLoaded ->
            Html.text "No game loaded"

        Loading ->
            Html.text "Loading game..."

        LoadFailed err ->
            Html.text "Error: Couldn't load game"


viewCommands : Game -> Html Msg
viewCommands game =
    let
        viewCommand : Command -> Html Msg
        viewCommand c =
            Html.button
                [ onClick
                    (ReceivedGameMsg
                        (Game.UserClickedCommandButton c)
                    )
                ]
                [ Html.text (Command.toName c) ]
    in
    Html.div
        [ Html.id "commands" ]
        (List.map viewCommand Command.listForMenu)


viewObjects : Game -> Html Msg
viewObjects game =
    let
        viewObject : Object -> Html Msg
        viewObject obj =
            Html.button
                [ onClick
                    (ReceivedGameMsg
                        (Game.UserClickedObject (Object.id obj))
                    )
                ]
                [ Html.text (Object.name obj) ]
    in
    Html.div
        [ Html.id "objects" ]
        [ Html.div [] [ viewObject (Game.player game) ]
        , Html.div [] (List.map viewObject (Game.objectsInInventory game))
        , Html.div [] [ viewObject (Game.currentRoom game) ]
        , Html.div [] (List.map viewObject (Game.objectsInCurrentRoom game))
        ]


viewNarration : Game -> Html Msg
viewNarration game =
    Html.div
        [ Html.id "narration" ]
        [ Html.text (Game.narration game)
        ]


viewParserInput : String -> Html Msg
viewParserInput input =
    Html.div []
        [ Html.textarea
            [ Html.Events.onInput UserTypedIntoParserInput
            , Html.value input
            , Html.rows 20
            , Html.style "width" "60ch"
            ]
            []
        , Html.pre []
            [ Html.text (Debug.toString <| Parser.run Vent.scriptParser input) ]
        ]


viewSaveButton : Game -> Html Msg
viewSaveButton game =
    Html.button
        [ onClick UserClickedSaveButton
        ]
        [ Html.text "Save Game Data" ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
