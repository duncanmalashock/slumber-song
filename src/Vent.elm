module Vent exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Ports
import RemoteData exposing (RemoteData)
import Vent.Command as Command exposing (Command)
import Vent.Effect as Effect exposing (Effect(..))
import Vent.Game as Game exposing (Game)
import Vent.Object as Object exposing (Object)
import Vent.ObjectStore as ObjectStore exposing (ObjectStore)
import Vent.VentScript as VentScript



-- VENT: the Vintage Exploratory-Narrative Toolkit


type alias Flags =
    { gameFile : String
    }


flagsDecoder : Decode.Decoder Flags
flagsDecoder =
    Decode.map Flags
        (Decode.field "gameFile" Decode.string)


type alias Model =
    { game : RemoteData Game.Game String
    }


type Msg
    = GameDataLoaded (List Object)
    | GameMsg Game.Msg
    | ReceivedDataFromJS Decode.Value


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialModel : Model
        initialModel =
            { game = Loading
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

        GameMsg gameMsg ->
            let
                ( updatedGame, effects ) =
                    updateLoadedGame gameMsg model.game
            in
            ( { model
                | game = updatedGame
              }
            , Ports.send effects
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receive ReceivedDataFromJS
