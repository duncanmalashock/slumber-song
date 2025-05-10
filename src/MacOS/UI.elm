module MacOS.UI exposing
    ( UI, new
    , createObject
    , attachObject
    , updateObject, updateList
    , removeObject
    , bringObjectToFront
    , getObject
    , pickObject, hitTest
    , mouseEventToHandlerMsg
    , view
    )

{-| A program's UI structure.


# UI

@docs UI, new


# Build/manipulate UI structure

@docs createObject
@docs attachObject


# Update

@docs updateObject, updateList
@docs removeObject
@docs bringObjectToFront


# Query

@docs getObject


# Picking Objects

@docs pickObject, hitTest


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
import MacOS.Rect as Rect exposing (Rect)
import MacOS.Screen as Screen exposing (Screen)
import MacOS.UI.Helpers as UIHelpers exposing (domIds)
import MacOS.UI.Object as UIObject exposing (Object)
import Set exposing (Set)


type UI msg
    = UI (Internals msg)


type alias ObjectId =
    String


type alias Internals msg =
    { uiObjects : Dict ObjectId (Object msg)
    , drawOrder : Dict ObjectId (List ObjectId)
    , objectToParent : Dict ObjectId ObjectId
    }


new : Screen -> UI msg
new screen =
    UI
        { uiObjects = Dict.empty
        , drawOrder = Dict.empty
        , objectToParent = Dict.empty
        }
        |> createObject
            (UIObject.new
                { id = domIds.root
                , rect = Screen.logical screen
                }
            )


removeObject : ObjectId -> UI msg -> UI msg
removeObject objId (UI internals) =
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


hitTest : Coordinate -> UI msg -> List String
hitTest coordinate (UI internals) =
    Dict.toList internals.uiObjects
        |> List.filterMap
            (\( key, uiObject ) ->
                if UIObject.hitTest coordinate uiObject then
                    Just key
                        |> Debug.log "hitTest"

                else
                    Nothing
            )


pickObject : List ObjectId -> UI msg -> Maybe ObjectId
pickObject candidates (UI internals) =
    let
        candidateSet : Set ObjectId
        candidateSet =
            Set.fromList candidates
                |> Debug.log "candidates"
    in
    [ ( domIds.windows, "" ) ]
        |> List.reverse
        |> List.filterMap
            (\( parentId, _ ) ->
                Dict.get parentId internals.drawOrder
                    |> Maybe.map List.reverse
            )
        |> List.concat
        |> List.filter (\id -> Set.member id candidateSet)
        |> List.head
        |> Debug.log "picked"


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


attachObject : { objectId : ObjectId, parentId : ObjectId, rect : Rect } -> UI msg -> UI msg
attachObject params (UI internals) =
    UI
        { internals
            | drawOrder =
                case Dict.get params.parentId internals.drawOrder of
                    Just _ ->
                        Dict.update params.parentId
                            (\maybeList ->
                                Maybe.map (\l -> l ++ [ params.objectId ]) maybeList
                            )
                            internals.drawOrder

                    Nothing ->
                        Dict.insert params.parentId [ params.objectId ] internals.drawOrder
            , objectToParent =
                Dict.insert params.objectId params.parentId internals.objectToParent
        }
        |> updateObject params.objectId (UIObject.setRect params.rect)


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
    List.foldl (\( key, updater ) -> updateObject key updater) interface newPairs


updateObject : ObjectId -> (Object msg -> Object msg) -> UI msg -> UI msg
updateObject objId updateObj (UI internals) =
    UI
        { internals
            | uiObjects =
                Dict.update objId
                    (\maybeObj -> Maybe.map updateObj maybeObj)
                    internals.uiObjects
        }


view : { debugObject : String } -> UI msg -> Html msg
view params ui =
    let
        rootNotFoundView : Html msg
        rootNotFoundView =
            div [ class "UI.view couldn't find `root` object" ]
                []
    in
    getObject ui domIds.root
        |> Maybe.map (viewHelp params ui)
        |> Maybe.withDefault rootNotFoundView


viewHelp : { debugObject : String } -> UI msg -> Object msg -> Html msg
viewHelp { debugObject } ui object =
    let
        childrenIds : List ObjectId
        childrenIds =
            getChildrenIds ui (UIObject.id object)

        childrenViews : List (Html msg)
        childrenViews =
            gatherChildrenViews { debugObject = debugObject } ui childrenIds []
                |> List.reverse
    in
    UIObject.view { debug = debugObject == UIObject.id object } object childrenViews


gatherChildrenViews : { debugObject : String } -> UI msg -> List ObjectId -> List (Html msg) -> List (Html msg)
gatherChildrenViews { debugObject } ui remainingIds viewsSoFar =
    case remainingIds of
        [] ->
            viewsSoFar

        objectId :: rest ->
            case getObject ui objectId of
                Just childObject ->
                    gatherChildrenViews { debugObject = debugObject } ui rest (viewHelp { debugObject = debugObject } ui childObject :: viewsSoFar)

                Nothing ->
                    gatherChildrenViews { debugObject = debugObject } ui rest viewsSoFar


getChildrenIds : UI msg -> ObjectId -> List ObjectId
getChildrenIds (UI internals) parentId =
    Dict.get parentId internals.drawOrder
        |> Maybe.withDefault []
