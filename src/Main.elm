module Main exposing (main)

import Browser
import FromJs
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Ports
import ToJs


type alias Model =
    { messages : List String
    }


type Msg
    = GotMessageFromJs FromJs.FromJs
    | ClickedSendButton


init : () -> ( Model, Cmd Msg )
init _ =
    ( { messages = [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMessageFromJs jsMsg ->
            let
                newMsg : String
                newMsg =
                    case jsMsg of
                        FromJs.Data str ->
                            "Data: " ++ str
            in
            ( { model | messages = newMsg :: model.messages }
            , Cmd.none
            )

        ClickedSendButton ->
            ( model
            , Ports.toJs (ToJs.encode (ToJs.Data "Hello from Elm!"))
            )


view : Model -> Html Msg
view model =
    div []
        [ div []
            (List.map text model.messages)
        , button
            [ onClick ClickedSendButton ]
            [ text "Send to JS" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    fromJsPortSubscription


fromJsPortSubscription : Sub Msg
fromJsPortSubscription =
    let
        decodeJsMsg : Decode.Value -> Msg
        decodeJsMsg val =
            case Decode.decodeValue FromJs.decoder val of
                Ok msg ->
                    GotMessageFromJs msg

                Err _ ->
                    GotMessageFromJs (FromJs.Data "Failed to decode")
    in
    Ports.fromJs decodeJsMsg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
