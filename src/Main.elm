module Main exposing (main)

import Browser
import Command
import Dict exposing (Dict)
import Exit
import FromJs
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
    = ReceivedMessageFromJs FromJs.FromJs


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
        ReceivedMessageFromJs fromJs ->
            case fromJs of
                FromJs.GameDataLoaded loadedRooms ->
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

                FromJs.UserClickedGoButton ->
                    let
                        ( updatedGame, effects ) =
                            Game.update (Game.UserClickedCommandButton Command.Go) model.game
                    in
                    ( { model
                        | game = updatedGame
                      }
                    , effectsToCmd model.interfaceMode effects
                    )

                FromJs.UserClickedExit toRoomId ->
                    let
                        ( updatedGame, effects ) =
                            Game.update (Game.UserClickedExit toRoomId) model.game
                    in
                    ( { model
                        | game = updatedGame
                      }
                    , effectsToCmd model.interfaceMode effects
                    )

                FromJs.DecodeError string ->
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
    Ports.receive ReceivedMessageFromJs


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
