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


type alias Model =
    { game : Game.Game
    }


type Msg
    = ReceivedMessageFromJs FromJs.FromJs


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialModel : Model
        initialModel =
            { game = Game.dummy
            }
    in
    ( initialModel
    , Ports.send
        [ ToJs.LoadGameData ]
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
                            ( { game = newGame
                              }
                            , List.map effectToJs effects
                                |> Ports.send
                            )

                        _ ->
                            ( model, Cmd.none )

                FromJs.UserClickedGoButton ->
                    ( { model
                        | game = Game.selectCommand Command.Go model.game
                      }
                    , Cmd.none
                    )

                FromJs.UserClickedExit toRoomId ->
                    case Game.selectedCommand model.game of
                        Just Command.Go ->
                            let
                                ( newGame, effects ) =
                                    Game.goToExit { toRoomId = toRoomId } model.game
                            in
                            ( { model
                                | game = newGame
                              }
                            , List.map effectToJs effects
                                |> Ports.send
                            )

                        _ ->
                            ( model
                            , Cmd.none
                            )

                FromJs.DecodeError string ->
                    ( model
                    , Cmd.none
                    )


effectToJs : Game.Effect -> ToJs.ToJs
effectToJs effect =
    case effect of
        Game.UpdateRoom room ->
            ToJs.UpdateRoom room

        Game.PlaySound filename ->
            ToJs.PlaySound filename


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receive ReceivedMessageFromJs


view : Model -> Html.Html Msg
view model =
    Html.text ""


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
