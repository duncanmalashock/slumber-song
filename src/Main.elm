module Main exposing (main)

import Browser
import FromJs
import Html
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Ports
import ToJs


type alias Model =
    { currentRoom : String
    }


type Msg
    = ReceivedMessageFromJs FromJs.FromJs
    | ClickedSendButton


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialModel : Model
        initialModel =
            { currentRoom = "entrance" }
    in
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedMessageFromJs fromJs ->
            let
                newMsg : String
                newMsg =
                    case fromJs of
                        FromJs.Data str ->
                            "Data: " ++ str
            in
            ( { model | currentRoom = newMsg }
            , Cmd.none
            )

        ClickedSendButton ->
            ( model
            , Ports.send (ToJs.Alert "Hello from Elm!")
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receive ReceivedMessageFromJs


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.button
            [ onClick ClickedSendButton ]
            [ Html.text "Send to JS" ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
