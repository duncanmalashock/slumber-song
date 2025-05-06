module MacOS.Interface exposing (Interface, OrderConstraint(..), addLayer, addToLayer, bringObjectToFront, containingCoordinate, get, msgForMouseEvent, new, remove, topmostFromList, update, view)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Mouse as Mouse
import MacOS.UIObject as UIObject exposing (UIObject)
import Set


type Interface msg
    = Interface (Internals msg)


type alias ObjectId =
    String


type alias LayerId =
    String


type alias Internals msg =
    { uiObjects : Dict ObjectId (UIObject msg)
    , drawOrder : Dict LayerId (List ObjectId)
    , layerOrder : List ( LayerId, Maybe OrderConstraint )
    , objectToLayer : Dict ObjectId LayerId
    }


new : Interface msg
new =
    Interface
        { uiObjects = Dict.empty
        , drawOrder = Dict.empty
        , layerOrder = []
        , objectToLayer = Dict.empty
        }


type OrderConstraint
    = AlwaysFirst
    | AlwaysLast


remove : ObjectId -> Interface msg -> Interface msg
remove objId (Interface internals) =
    let
        removeFromUiObjects : Internals msg -> Internals msg
        removeFromUiObjects i =
            { i
                | uiObjects =
                    Dict.remove objId i.uiObjects
            }

        removeFromDrawOrder : Internals msg -> Internals msg
        removeFromDrawOrder i =
            { i
                | drawOrder =
                    i.drawOrder
                        |> Dict.toList
                        |> List.map
                            (\( key, value ) ->
                                ( key, value |> List.filter (\item -> item /= objId) )
                            )
                        |> Dict.fromList
            }

        removeFromObjectToLayer : Internals msg -> Internals msg
        removeFromObjectToLayer i =
            { i
                | objectToLayer =
                    Dict.remove objId i.objectToLayer
            }
    in
    internals
        |> removeFromUiObjects
        |> removeFromDrawOrder
        |> removeFromObjectToLayer
        |> Interface


addLayer : { id : LayerId, orderConstraint : Maybe OrderConstraint } -> Interface msg -> Interface msg
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


topmostFromList : List ObjectId -> Interface msg -> Maybe ObjectId
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


addToLayer : LayerId -> List ( ObjectId, UIObject msg ) -> Interface msg -> Interface msg
addToLayer layerId newPairs interface =
    List.foldl (addSingle layerId) interface newPairs


addSingle : LayerId -> ( ObjectId, UIObject msg ) -> Interface msg -> Interface msg
addSingle layerId ( objectId, obj ) (Interface internals) =
    Interface
        { internals
            | uiObjects = Dict.insert objectId obj internals.uiObjects
            , drawOrder =
                Dict.update layerId
                    (\maybeList -> Maybe.map (\l -> l ++ [ objectId ]) maybeList)
                    internals.drawOrder
            , objectToLayer = Dict.insert objectId layerId internals.objectToLayer
        }


containingCoordinate : Coordinate -> Interface msg -> List String
containingCoordinate coordinate (Interface internals) =
    Dict.toList internals.uiObjects
        |> List.filterMap
            (\( key, uiObject ) ->
                if UIObject.containsCoordinate coordinate uiObject then
                    Just key

                else
                    Nothing
            )


msgForMouseEvent : Mouse.Event -> Interface msg -> Maybe msg
msgForMouseEvent mouseEvent ((Interface internals) as interface) =
    case mouseEvent of
        Mouse.MouseDown objId ->
            case get objId interface of
                Just obj ->
                    UIObject.getMouseEventHandler mouseEvent obj

                Nothing ->
                    Nothing

        Mouse.MouseUp ->
            Nothing

        Mouse.Click objId ->
            case get objId interface of
                Just obj ->
                    UIObject.getMouseEventHandler mouseEvent obj

                Nothing ->
                    Nothing

        Mouse.DoubleClick objId ->
            case get objId interface of
                Just obj ->
                    UIObject.getMouseEventHandler mouseEvent obj

                Nothing ->
                    Nothing

        Mouse.DragStart objId ->
            case get objId interface of
                Just obj ->
                    UIObject.getMouseEventHandler mouseEvent obj

                Nothing ->
                    Nothing


get : ObjectId -> Interface msg -> Maybe (UIObject msg)
get objId (Interface internals) =
    Dict.get objId internals.uiObjects


bringObjectToFront : ObjectId -> Interface msg -> Interface msg
bringObjectToFront objectId (Interface internals) =
    case Dict.get objectId internals.objectToLayer of
        Just layerId ->
            let
                updatedDrawOrder =
                    Dict.update layerId
                        (Maybe.map
                            (\ids ->
                                List.filter ((/=) objectId) ids ++ [ objectId ]
                            )
                        )
                        internals.drawOrder
            in
            Interface { internals | drawOrder = updatedDrawOrder }

        Nothing ->
            Interface internals


update : ObjectId -> (UIObject msg -> UIObject msg) -> Interface msg -> Interface msg
update objId updateObj (Interface internals) =
    Interface
        { internals
            | uiObjects =
                Dict.update objId
                    (\maybeObj -> Maybe.map updateObj maybeObj)
                    internals.uiObjects
        }


view : Interface msg -> Html msg
view (Interface internals) =
    internals.layerOrder
        |> List.filterMap (\( layerId, _ ) -> Dict.get layerId internals.drawOrder)
        |> List.concat
        |> List.filterMap (\objectId -> Dict.get objectId internals.uiObjects)
        |> List.map UIObject.view
        |> Html.div []
