module MacOS.UI exposing
    ( UI, new
    , createObject
    , attachObject
    , bringObjectToFront
    , remove
    , update, updateList
    , getObject
    , topmostObjectInList, hitTest
    , mouseEventToHandlerMsg
    , view
    )

{-| A program's UI structure.


# UI

@docs UI, new


# Build/manipulate UI structure

@docs createObject
@docs attachObject
@docs bringObjectToFront
@docs remove


# Update

@docs update, updateList


# Query

@docs getObject


# Picking Objects

@docs topmostObjectInList, hitTest


# Events

@docs mouseEventToHandlerMsg


# View

@docs view

-}

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Mouse as Mouse
import MacOS.Rect as Rect
import MacOS.UI.Helpers as UIHelpers exposing (domIds)
import MacOS.UI.Object as UIObject exposing (Object)
import Set


type UI msg
    = UI (Internals msg)


type alias ObjectId =
    String


type alias Internals msg =
    { uiObjects : Dict ObjectId (Object msg)
    , drawOrder : Dict ObjectId (List ObjectId)
    , objectToParent : Dict ObjectId ObjectId
    }


new : UI msg
new =
    UI
        { uiObjects = Dict.empty
        , drawOrder = Dict.empty
        , objectToParent = Dict.empty
        }
        |> createObject
            (UIObject.new
                { id = domIds.root
                , rect = Rect.new ( 0, 0 ) ( 512, 342 )
                }
            )


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

        removeFromObjectToParent : Internals msg -> Internals msg
        removeFromObjectToParent i =
            { i
                | objectToParent =
                    Dict.remove objId i.objectToParent
            }
    in
    internals
        |> removeFromUiObjects
        |> removeFromDrawOrder
        |> removeFromObjectToParent
        |> UI


topmostObjectInList : List ObjectId -> UI msg -> Maybe ObjectId
topmostObjectInList candidates (UI internals) =
    let
        candidateSet =
            Set.fromList candidates
    in
    []
        -- internals.layerOrder
        |> List.reverse
        |> List.filterMap
            (\( layerId, _ ) ->
                Dict.get layerId internals.drawOrder
                    |> Maybe.map List.reverse
            )
        |> List.concat
        |> List.filter (\id -> Set.member id candidateSet)
        |> List.head


createObject : Object msg -> UI msg -> UI msg
createObject object (UI internals) =
    let
        objectId =
            UIObject.id object
    in
    UI
        { internals
            | uiObjects = Dict.insert objectId object internals.uiObjects
        }


attachObject : { objectId : ObjectId, parentId : ObjectId } -> UI msg -> UI msg
attachObject { objectId, parentId } (UI internals) =
    -- User could attach an object that hasn't been created yet. What to do??
    UI
        { internals
            | drawOrder =
                case Dict.get parentId internals.drawOrder of
                    Just _ ->
                        Dict.update parentId
                            (\maybeList ->
                                Maybe.map (\l -> l ++ [ objectId ]) maybeList
                            )
                            internals.drawOrder

                    Nothing ->
                        Dict.insert parentId [ objectId ] internals.drawOrder
            , objectToParent =
                Dict.insert objectId parentId internals.objectToParent
        }


hitTest : Coordinate -> UI msg -> List String
hitTest coordinate (UI internals) =
    Dict.toList internals.uiObjects
        |> List.filterMap
            (\( key, uiObject ) ->
                if UIObject.hitTest coordinate uiObject then
                    Just key

                else
                    Nothing
            )


mouseEventToHandlerMsg : Mouse.Event -> UI msg -> Maybe msg
mouseEventToHandlerMsg mouseEvent ((UI internals) as ui) =
    case mouseEvent of
        Mouse.MouseDown objId ->
            case getObject ui objId of
                Just obj ->
                    UIObject.getMouseEventHandler mouseEvent obj

                Nothing ->
                    Nothing

        Mouse.MouseUp ->
            Nothing

        Mouse.Click objId ->
            case getObject ui objId of
                Just obj ->
                    UIObject.getMouseEventHandler mouseEvent obj

                Nothing ->
                    Nothing

        Mouse.DoubleClick objId ->
            case getObject ui objId of
                Just obj ->
                    UIObject.getMouseEventHandler mouseEvent obj

                Nothing ->
                    Nothing

        Mouse.DragStart objId ->
            case getObject ui objId of
                Just obj ->
                    UIObject.getMouseEventHandler mouseEvent obj

                Nothing ->
                    Nothing


getObject : UI msg -> ObjectId -> Maybe (Object msg)
getObject (UI internals) objId =
    Dict.get objId internals.uiObjects


bringObjectToFront : ObjectId -> UI msg -> UI msg
bringObjectToFront objectId (UI internals) =
    case Dict.get objectId internals.objectToParent of
        Just parentId ->
            let
                updatedDrawOrder =
                    Dict.update parentId
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
view ui =
    let
        rootNotFoundView : Html msg
        rootNotFoundView =
            div [ class "UI.view couldn't find `root` object" ]
                []
    in
    getObject ui domIds.root
        |> Maybe.map (viewHelp ui)
        |> Maybe.withDefault rootNotFoundView


viewHelp : UI msg -> Object msg -> Html msg
viewHelp ui object =
    let
        childrenIds : List ObjectId
        childrenIds =
            getChildrenIds ui (UIObject.id object)

        childrenViews : List (Html msg)
        childrenViews =
            gatherChildrenViews ui childrenIds []
                |> List.reverse
    in
    UIObject.view object childrenViews


gatherChildrenViews : UI msg -> List ObjectId -> List (Html msg) -> List (Html msg)
gatherChildrenViews ui remainingIds viewsSoFar =
    case remainingIds of
        [] ->
            viewsSoFar

        objectId :: rest ->
            case getObject ui objectId of
                Just childObject ->
                    gatherChildrenViews ui rest (viewHelp ui childObject :: viewsSoFar)

                Nothing ->
                    gatherChildrenViews ui rest viewsSoFar


getChildrenIds : UI msg -> ObjectId -> List ObjectId
getChildrenIds (UI internals) parentId =
    Dict.get parentId internals.drawOrder
        |> Maybe.withDefault []
