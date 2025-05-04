module MacOS.Interface exposing (Interface, add, containingCoordinate, get, new, update, view)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.UIObject as UIObject exposing (UIObject)


type Interface
    = Interface Internals


type alias Internals =
    { dict : Dict String UIObject
    }


new : Interface
new =
    Interface
        { dict = Dict.empty
        }


add : List ( String, UIObject ) -> Interface -> Interface
add newPairs interface =
    List.foldl addSingle interface newPairs


addSingle : ( String, UIObject ) -> Interface -> Interface
addSingle ( id, obj ) (Interface internals) =
    Interface
        { dict = Dict.insert id obj internals.dict
        }


containingCoordinate : Coordinate -> Interface -> List String
containingCoordinate coordinate (Interface internals) =
    Dict.toList internals.dict
        |> List.filterMap
            (\( key, uiObject ) ->
                if UIObject.containsCoordinate coordinate uiObject then
                    Just key

                else
                    Nothing
            )


get : String -> Interface -> Maybe UIObject
get objId (Interface internals) =
    Dict.get objId internals.dict


update : String -> (UIObject -> UIObject) -> Interface -> Interface
update objId updateObj (Interface internals) =
    Interface
        { internals
            | dict =
                Dict.update objId
                    (\maybeObj -> Maybe.map updateObj maybeObj)
                    internals.dict
        }


view : Interface -> Html msg
view (Interface internals) =
    Dict.toList internals.dict
        |> List.map Tuple.second
        |> List.map UIObject.view
        |> Html.div
            []
