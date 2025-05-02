module MacOS.Context exposing (Context)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)


type alias Context msg =
    { listenersForObject :
        { id : String, coordinate : Coordinate } -> List (Attribute msg)
    }
