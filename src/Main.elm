module Main exposing (main)

import Browser
import Command
import Dict exposing (Dict)
import Effect exposing (Effect)
import Exit
import Game
import Html
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Map exposing (Map)
import Ports
import Room exposing (Room)


type alias Flags =
    { useVDomInterface : Bool
    }


flagsDecoder : Decode.Decoder Flags
flagsDecoder =
    Decode.map Flags
        (Decode.field "useVDomInterface" Decode.bool)


type alias Model =
    { game : RemoteData Game.Game String
    , interfaceMode : InterfaceMode
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
    = GameDataLoaded (List Room.Room)
    | ReceivedGameMsg Game.Msg
    | GameMsgDecodeError Decode.Error


msgDecoder : Decode.Decoder Msg
msgDecoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "GameDataLoaded" ->
                        Decode.field "payload"
                            (Decode.map GameDataLoaded (Decode.list Room.decoder))

                    "UserClickedGoButton" ->
                        Decode.succeed
                            (ReceivedGameMsg
                                (Game.UserClickedCommandButton Command.Go)
                            )

                    "UserClickedExit" ->
                        Decode.field "payload"
                            (Decode.map
                                (\toRoomId ->
                                    ReceivedGameMsg
                                        (Game.UserClickedExit toRoomId)
                                )
                                (Decode.field "toRoomId" Decode.string)
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
            }
    in
    ( initialModel
    , Ports.send
        [ Effect.LoadGameData
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
        GameDataLoaded loadedRooms ->
            case loadedRooms of
                (initialRoom :: _) as rooms ->
                    let
                        ( newGame, effects ) =
                            Game.new rooms initialRoom
                    in
                    ( { model
                        | game = LoadSuccessful newGame
                      }
                    , effectsToCmd effects
                    )

                _ ->
                    ( { model
                        | game = LoadFailed "Couldn't load game"
                      }
                    , effectsToCmd
                        [ Effect.ReportError "Couldn't load game" ]
                    )

        ReceivedGameMsg gameMsg ->
            let
                ( updatedGame, effects ) =
                    updateLoadedGame gameMsg model.game
            in
            ( { model
                | game = updatedGame
              }
            , effectsToCmd effects
            )

        GameMsgDecodeError decodeError ->
            ( model
            , effectsToCmd
                [ Effect.ReportError <|
                    "Couldn't decode msg: "
                        ++ Decode.errorToString decodeError
                ]
            )


effectsToCmd : List Effect -> Cmd Msg
effectsToCmd effects =
    effects
        |> Ports.send


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


view : Model -> Html.Html Msg
view model =
    case model.interfaceMode of
        InterfaceJS ->
            Html.text ""

        InterfaceElmVDom ->
            elmView model


elmView : Model -> Html.Html Msg
elmView model =
    Html.text "Using Elm VDOM"


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
