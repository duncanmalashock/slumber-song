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
    }


type Msg
    = ReceivedMessageFromJs FromJs.FromJs


init : () -> ( Model, Cmd Msg )
init _ =
    let
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

        initialModel : Model
        initialModel =
            { currentRoom = entrance
            , map =
                Map.new
                    [ entrance
                    , hallway
                    ]
            }
    in
    ( initialModel
    , Ports.send (ToJs.RoomChanged entrance)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedMessageFromJs fromJs ->
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
