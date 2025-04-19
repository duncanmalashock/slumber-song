module Main exposing (main)

import Browser
import Command
import Dict exposing (Dict)
import Exit
import Game
import Html
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Map exposing (Map)
import Ports
import Room exposing (Room)
import ToJs


type alias Flags =
    { useVDomInterface : Bool
    }


flagsDecoder : Decode.Decoder Flags
flagsDecoder =
    Decode.map Flags
        (Decode.field "useVDomInterface" Decode.bool)


type alias Model =
    { game : Game.Game
    , interfaceMode : InterfaceMode
    }


type InterfaceMode
    = InterfaceJS
    | InterfaceElmVDom


type Msg
    = GameDataLoaded (List Room.Room)
    | UserClickedGoButton
    | UserClickedExit String
    | DecodeError Decode.Error


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
                        Decode.succeed UserClickedGoButton

                    "UserClickedExit" ->
                        Decode.field "payload"
                            (Decode.map UserClickedExit (Decode.field "toRoomId" Decode.string))

                    _ ->
                        Decode.fail "Unknown tag"
            )


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialModel : Model
        initialModel =
            { game = Game.dummy
            , interfaceMode =
                if flags.useVDomInterface then
                    InterfaceElmVDom

                else
                    InterfaceJS
            }
    in
    ( initialModel
    , Ports.send
        [ ToJs.LoadGameData
        ]
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
                        | game = newGame
                      }
                    , effectsToCmd model.interfaceMode effects
                    )

                _ ->
                    ( model, Cmd.none )

        UserClickedGoButton ->
            let
                ( updatedGame, effects ) =
                    Game.update (Game.UserClickedCommandButton Command.Go) model.game
            in
            ( { model
                | game = updatedGame
              }
            , effectsToCmd model.interfaceMode effects
            )

        UserClickedExit toRoomId ->
            let
                ( updatedGame, effects ) =
                    Game.update (Game.UserClickedExit toRoomId) model.game
            in
            ( { model
                | game = updatedGame
              }
            , effectsToCmd model.interfaceMode effects
            )

        DecodeError string ->
            ( model
            , Cmd.none
            )


effectsToCmd : InterfaceMode -> List Game.Effect -> Cmd Msg
effectsToCmd interfaceMode effects =
    case interfaceMode of
        InterfaceJS ->
            effects
                |> List.map effectToJs
                |> Ports.send

        InterfaceElmVDom ->
            Cmd.none


effectToJs : Game.Effect -> ToJs.ToJs
effectToJs effect =
    case effect of
        Game.UpdateRoom room ->
            ToJs.UpdateRoom room

        Game.PlaySound filename ->
            ToJs.PlaySound filename

        Game.HighlightCommand command ->
            ToJs.HighlightCommand command


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receive jsonToMsg


jsonToMsg : Decode.Value -> Msg
jsonToMsg json =
    case Decode.decodeValue msgDecoder json of
        Ok msg ->
            msg

        Err err ->
            DecodeError err


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
