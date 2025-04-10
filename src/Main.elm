port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode



-- TYPES


type alias Model =
    { messages : List String }


type Msg
    = GotMessageFromJs JsMessage
    | SendMessageToJs



-- PORTS


port toJs : Encode.Value -> Cmd msg


port fromJs : (Decode.Value -> msg) -> Sub msg


type JsMessage
    = Alert String
    | Data String


decodeJsMessage : Decode.Value -> Msg
decodeJsMessage val =
    case Decode.decodeValue jsMessageDecoder val of
        Ok msg ->
            GotMessageFromJs msg

        Err _ ->
            GotMessageFromJs (Data "Failed to decode")


jsMessageDecoder : Decode.Decoder JsMessage
jsMessageDecoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "alert" ->
                        Decode.map Alert (Decode.field "message" Decode.string)

                    "data" ->
                        Decode.map Data (Decode.field "payload" Decode.string)

                    _ ->
                        Decode.fail "Unknown tag"
            )



-- ENCODE


encodeMessageToJs : JsMessage -> Encode.Value
encodeMessageToJs msg =
    case msg of
        Alert s ->
            Encode.object
                [ ( "tag", Encode.string "alert" )
                , ( "message", Encode.string s )
                ]

        Data s ->
            Encode.object
                [ ( "tag", Encode.string "data" )
                , ( "payload", Encode.string s )
                ]



-- INIT / UPDATE / VIEW


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
                        Alert str ->
                            "Alert: " ++ str

                        Data str ->
                            "Data: " ++ str
            in
            ( { model | messages = newMsg :: model.messages }, Cmd.none )

        SendMessageToJs ->
            let
                toJsPayload =
                    Data "Hello from Elm!"
            in
            ( model, toJs (encodeMessageToJs toJsPayload) )


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
    fromJs decodeJsMessage


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
