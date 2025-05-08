module MacOS.UI exposing
    ( UI, new
    , createObject
    , addLayer, OrderConstraint(..)
    , attachObject, addToLayer
    , bringObjectToFront
    , remove
    , update, updateList
    , get, topmostFromList, containingCoordinate, msgForMouseEvent
    , view
    )

{-| A program's UI structure.


# UI

@docs UI, new


# Build/manipulate UI structure

@docs createObject
@docs addLayer, OrderConstraint
@docs attachObject, addToLayer
@docs bringObjectToFront
@docs remove


# Update

@docs update, updateList


# Query

@docs get, topmostFromList, containingCoordinate, msgForMouseEvent


# View

@docs view

-}

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Mouse as Mouse
import MacOS.UI.Object as Object exposing (Object)
import Set


type UI msg
    = UI (Internals msg)


type alias ObjectId =
    String


type alias LayerId =
    String


type alias Internals msg =
    { uiObjects : Dict ObjectId (Object msg)
    , drawOrder : Dict LayerId (List ObjectId)
    , layerOrder : List ( LayerId, Maybe OrderConstraint )
    , objectToLayer : Dict ObjectId LayerId
    }


new : UI msg
new =
    UI
        { uiObjects = Dict.empty
        , drawOrder = Dict.empty
        , layerOrder = []
        , objectToLayer = Dict.empty
        }


type OrderConstraint
    = AlwaysFirst
    | AlwaysLast


remove : ObjectId -> UI msg -> UI msg
remove objId (UI internals) =
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
        |> UI


addLayer : { id : LayerId, orderConstraint : Maybe OrderConstraint } -> UI msg -> UI msg
addLayer { id, orderConstraint } (UI internals) =
    let
        newLayerOrder : List ( LayerId, Maybe OrderConstraint )
        newLayerOrder =
            internals.layerOrder
                ++ [ ( id, orderConstraint ) ]
                |> List.sortWith orderLayers
    in
    UI
        { internals
            | layerOrder = newLayerOrder
            , drawOrder = Dict.insert id [] internals.drawOrder
        }


topmostFromList : List ObjectId -> UI msg -> Maybe ObjectId
topmostFromList candidates (UI internals) =
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


addToLayer : LayerId -> List ( ObjectId, Object msg ) -> UI msg -> UI msg
addToLayer layerId newPairs interface =
    List.foldl (addSingle layerId) interface newPairs


addSingle : LayerId -> ( ObjectId, Object msg ) -> UI msg -> UI msg
addSingle layerId ( objectId, obj ) (UI internals) =
    UI
        { internals
            | uiObjects = Dict.insert objectId obj internals.uiObjects
            , drawOrder =
                Dict.update layerId
                    (\maybeList -> Maybe.map (\l -> l ++ [ objectId ]) maybeList)
                    internals.drawOrder
            , objectToLayer = Dict.insert objectId layerId internals.objectToLayer
        }


createObject : ObjectId -> Object msg -> UI msg -> UI msg
createObject objectId object (UI internals) =
    UI
        { internals
            | uiObjects = Dict.insert objectId object internals.uiObjects
        }


attachObject : { objectId : ObjectId, layerId : LayerId } -> UI msg -> UI msg
attachObject { objectId, layerId } (UI internals) =
    -- User could attach an object that hasn't been created yet. What to do??
    UI
        { internals
            | drawOrder =
                Dict.update layerId
                    (\maybeList -> Maybe.map (\l -> l ++ [ objectId ]) maybeList)
                    internals.drawOrder
            , objectToLayer = Dict.insert objectId layerId internals.objectToLayer
        }
        |> Debug.log "TODO"


containingCoordinate : Coordinate -> UI msg -> List String
containingCoordinate coordinate (UI internals) =
    Dict.toList internals.uiObjects
        |> List.filterMap
            (\( key, uiObject ) ->
                if Object.containsCoordinate coordinate uiObject then
                    Just key

                else
                    Nothing
            )


msgForMouseEvent : Mouse.Event -> UI msg -> Maybe msg
msgForMouseEvent mouseEvent ((UI internals) as interface) =
    case mouseEvent of
        Mouse.MouseDown objId ->
            case get objId interface of
                Just obj ->
                    Object.getMouseEventHandler mouseEvent obj

                Nothing ->
                    Nothing

        Mouse.MouseUp ->
            Nothing

        Mouse.Click objId ->
            case get objId interface of
                Just obj ->
                    Object.getMouseEventHandler mouseEvent obj

                Nothing ->
                    Nothing

        Mouse.DoubleClick objId ->
            case get objId interface of
                Just obj ->
                    Object.getMouseEventHandler mouseEvent obj

                Nothing ->
                    Nothing

        Mouse.DragStart objId ->
            case get objId interface of
                Just obj ->
                    Object.getMouseEventHandler mouseEvent obj

                Nothing ->
                    Nothing


get : ObjectId -> UI msg -> Maybe (Object msg)
get objId (UI internals) =
    Dict.get objId internals.uiObjects


bringObjectToFront : ObjectId -> UI msg -> UI msg
bringObjectToFront objectId (UI internals) =
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
            UI { internals | drawOrder = updatedDrawOrder }

        Nothing ->
            UI internals


updateList : List ( ObjectId, Object msg -> Object msg ) -> UI msg -> UI msg
updateList newPairs interface =
    List.foldl (\( key, updater ) -> update key updater) interface newPairs


update : ObjectId -> (Object msg -> Object msg) -> UI msg -> UI msg
update objId updateObj (UI internals) =
    UI
        { internals
            | uiObjects =
                Dict.update objId
                    (\maybeObj -> Maybe.map updateObj maybeObj)
                    internals.uiObjects
        }


view : UI msg -> Html msg
view (UI internals) =
    internals.layerOrder
        |> List.filterMap (\( layerId, _ ) -> Dict.get layerId internals.drawOrder)
        |> List.concat
        |> List.filterMap (\objectId -> Dict.get objectId internals.uiObjects)
        |> List.map Object.view
        |> Html.div []
