module MacOS.Interface exposing (Interface, OrderConstraint(..), addLayer, addToLayer, containingCoordinate, get, new, topmostFromList, update, view)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.UIObject as UIObject exposing (UIObject)
import Set


type Interface
    = Interface Internals


type alias ObjectId =
    String


type alias LayerId =
    String


type alias Internals =
    { uiObjects : Dict ObjectId UIObject
    , drawOrder : Dict LayerId (List ObjectId)
    , layerOrder : List ( LayerId, Maybe OrderConstraint )
    }


new : Interface
new =
    Interface
        { uiObjects = Dict.empty
        , drawOrder = Dict.empty
        , layerOrder = []
        }


type OrderConstraint
    = AlwaysFirst
    | AlwaysLast


addLayer : { id : LayerId, orderConstraint : Maybe OrderConstraint } -> Interface -> Interface
addLayer { id, orderConstraint } (Interface internals) =
    let
        newLayerOrder : List ( LayerId, Maybe OrderConstraint )
        newLayerOrder =
            internals.layerOrder
                ++ [ ( id, orderConstraint ) ]
                |> List.sortWith orderLayers
    in
    Interface
        { internals
            | layerOrder = newLayerOrder
            , drawOrder = Dict.insert id [] internals.drawOrder
        }


topmostFromList : List ObjectId -> Interface -> Maybe ObjectId
topmostFromList candidates (Interface internals) =
    let
        candidateSet =
            Set.fromList candidates
    in
    internals.layerOrder
        |> List.reverse
        |> List.filterMap
            (\( layerId, _ ) ->
                Dict.get layerId internals.drawOrder
                    |> Maybe.map List.reverse
            )
        |> List.concat
        |> List.filter (\id -> Set.member id candidateSet)
        |> List.head


orderLayers : ( LayerId, Maybe OrderConstraint ) -> ( LayerId, Maybe OrderConstraint ) -> Order
orderLayers ( l1, oc1 ) ( l2, oc2 ) =
    case ( oc1, oc2 ) of
        ( Just AlwaysFirst, _ ) ->
            LT

        ( Just AlwaysLast, _ ) ->
            GT

        ( _, Just AlwaysFirst ) ->
            GT

        ( _, Just AlwaysLast ) ->
            LT

        _ ->
            EQ


addToLayer : LayerId -> List ( ObjectId, UIObject ) -> Interface -> Interface
addToLayer layerId newPairs interface =
    List.foldl (addSingle layerId) interface newPairs


addSingle : LayerId -> ( ObjectId, UIObject ) -> Interface -> Interface
addSingle layerId ( objectId, obj ) (Interface internals) =
    Interface
        { internals
            | uiObjects = Dict.insert objectId obj internals.uiObjects
            , drawOrder =
                Dict.update layerId
                    (\maybeList -> Maybe.map (\l -> List.append l [ objectId ]) maybeList)
                    internals.drawOrder
        }


containingCoordinate : Coordinate -> Interface -> List String
containingCoordinate coordinate (Interface internals) =
    Dict.toList internals.uiObjects
        |> List.filterMap
            (\( key, uiObject ) ->
                if UIObject.containsCoordinate coordinate uiObject then
                    Just key

                else
                    Nothing
            )


get : ObjectId -> Interface -> Maybe UIObject
get objId (Interface internals) =
    Dict.get objId internals.uiObjects


update : ObjectId -> (UIObject -> UIObject) -> Interface -> Interface
update objId updateObj (Interface internals) =
    Interface
        { internals
            | uiObjects =
                Dict.update objId
                    (\maybeObj -> Maybe.map updateObj maybeObj)
                    internals.uiObjects
        }


view : Interface -> Html msg
view (Interface internals) =
    Dict.toList internals.uiObjects
        |> List.map Tuple.second
        |> List.map UIObject.view
        |> Html.div
            []
