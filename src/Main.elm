module Main exposing (main)

import Browser
import Dict exposing (Dict)
import FromJs
import Html
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Ports
import Room exposing (Room)
import ToJs


type alias Model =
    { currentRoom : Room
    }


type Msg
    = ReceivedMessageFromJs FromJs.FromJs


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialRoom : Room
        initialRoom =
            Room.new
                { id = "entrance"
                , name = "Entrance"
                }

        initialModel : Model
        initialModel =
            { currentRoom = initialRoom
            }
    in
    ( initialModel
    , Ports.send (ToJs.RoomChanged initialRoom)
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
