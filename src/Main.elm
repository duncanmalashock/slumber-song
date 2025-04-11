module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Exit
import FromJs
import Html
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Map exposing (Map)
import Ports
import Room exposing (Room)
import ToJs


type alias Model =
    { currentRoom : Room
    , map : Map
    , selectedCommand : Maybe Command
    }


type Command
    = Go


type Msg
    = ReceivedMessageFromJs FromJs.FromJs


entrance : Room
entrance =
    Room.new
        { id = "entrance"
        , name = "Entrance"
        , exits =
            [ Exit.new
                { toRoomId = "hallway"
                }
            ]
        }


hallway : Room
hallway =
    Room.new
        { id = "hallway"
        , name = "Hallway"
        , exits =
            [ Exit.new
                { toRoomId = "entrance"
                }
            ]
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialModel : Model
        initialModel =
            { currentRoom = entrance
            , map =
                Map.new
                    [ entrance
                    , hallway
                    ]
            , selectedCommand = Nothing
            }
    in
    ( initialModel
    , Ports.send [ ToJs.UpdateRoom entrance ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedMessageFromJs fromJs ->
            case fromJs of
                FromJs.UserClickedGoButton ->
                    ( { model
                        | selectedCommand = Just Go
                      }
                    , Cmd.none
                    )

                FromJs.UserClickedExit toRoomId ->
                    case model.selectedCommand of
                        Just Go ->
                            let
                                newRoom : Room
                                newRoom =
                                    model.map
                                        |> Map.getRoomById toRoomId
                                        |> Maybe.withDefault model.currentRoom
                            in
                            ( { model
                                | currentRoom = newRoom
                                , selectedCommand = Nothing
                              }
                            , Ports.send <|
                                [ ToJs.UpdateRoom newRoom
                                ]
                                    ++ (if Room.id newRoom == "hallway" then
                                            [ ToJs.PlaySound "boing" ]

                                        else
                                            []
                                       )
                            )

                        _ ->
                            ( model
                            , Cmd.none
                            )

                FromJs.DecodeError string ->
                    ( model
                    , Cmd.none
                    )


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
