port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events
import Time


port playSound : String -> Cmd msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { current : Int
    }


initialTime : Int
initialTime =
    30


init : () -> ( Model, Cmd Msg )
init flags =
    ( { current = initialTime
      }
    , Cmd.none
    )


type Msg
    = ClickedBoingButton


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedBoingButton ->
            ( { model | current = initialTime }
            , playSound "boing"
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [ class "row gap_8" ]
        [ viewButton "Boing" ClickedBoingButton ]


viewButton : String -> Msg -> Html Msg
viewButton label onClickMsg =
    button [ Html.Events.onClick onClickMsg ] [ text label ]
