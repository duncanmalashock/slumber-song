module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Ports


type alias Model =
    { messages : List String
    }


type Msg
    = GotMessageFromJs Ports.JsMessage
    | SendMessageToJs


init : () -> ( Model, Cmd Msg )
init _ =
    ( { messages = [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMessageFromJs jsMsg ->
            let
                newMsg =
                    case jsMsg of
                        Ports.Alert str ->
                            "Alert: " ++ str

                        Ports.Data str ->
                            "Data: " ++ str
            in
            ( { model | messages = newMsg :: model.messages }, Cmd.none )

        SendMessageToJs ->
            let
                toJsPayload =
                    Ports.Data "Hello from Elm!"
            in
            ( model, Ports.toJs (Ports.encodeMessageToJs toJsPayload) )


view : Model -> Html Msg
view model =
    div []
        [ div []
            (List.map text model.messages)
        , button
            [ onClick SendMessageToJs ]
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
            case Decode.decodeValue Ports.jsMessageDecoder val of
                Ok msg ->
                    GotMessageFromJs msg

                Err _ ->
                    GotMessageFromJs (Ports.Data "Failed to decode")
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
