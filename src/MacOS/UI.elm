module MacOS.UI exposing
    ( UI, new
    , createObject
    , attachObject
    , updateObject, updateObjectList
    , removeObject
    , bringObjectToFront
    , getObject
    , hitTest, pickTopmostObject
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

@docs updateObject, updateObjectList
@docs removeObject
@docs bringObjectToFront


# Query

@docs getObject


# Picking Objects

@docs hitTest, pickTopmostObject


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
import MacOS.UI.View.Rectangle exposing (Config(..))
import Set exposing (Set)


type UI msg
    = UI (Internals msg)


type alias ObjectId =
    String


type alias Internals msg =
    { uiObjects : Dict ObjectId (Object msg)

    -- childrenInDrawOrder is in order from bottom to top
    , childrenInDrawOrder : Dict ObjectId (List ObjectId)
    , objectToParent : Dict ObjectId ObjectId
    , absoluteRects : Dict ObjectId Rect
    }


new : Screen -> UI msg
new screen =
    UI
        { uiObjects = Dict.empty
        , childrenInDrawOrder = Dict.empty
        , objectToParent = Dict.empty
        , absoluteRects = Dict.empty
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
                | childrenInDrawOrder =
                    i.childrenInDrawOrder
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


getAbsoluteRect : UI msg -> ObjectId -> Maybe Rect
getAbsoluteRect ((UI internals) as ui) objectId =
    Dict.get objectId internals.absoluteRects


updateAbsoluteRectsForDescendants : ObjectId -> UI msg -> UI msg
updateAbsoluteRectsForDescendants objectId (UI internals) =
    let
        updateDescendants : ObjectId -> Dict ObjectId Rect -> Dict ObjectId Rect
        updateDescendants currentId currentAbsoluteRects =
            let
                maybeParentRect : Maybe Rect
                maybeParentRect =
                    Dict.get currentId internals.objectToParent
                        |> Maybe.andThen (\parentId -> Dict.get parentId currentAbsoluteRects)

                maybeCurrentObject : Maybe (Object msg)
                maybeCurrentObject =
                    Dict.get currentId internals.uiObjects

                currentLocalRect : Rect
                currentLocalRect =
                    maybeCurrentObject
                        |> Maybe.map UIObject.rect
                        |> Maybe.withDefault (Rect.new ( 0, 0 ) ( 0, 0 ))

                newAbsoluteRect : Rect
                newAbsoluteRect =
                    case maybeParentRect of
                        Just parentRect ->
                            let
                                -- Rect x y w h = currentLocalRect
                                x : Int
                                x =
                                    Rect.posX currentLocalRect

                                y : Int
                                y =
                                    Rect.posY currentLocalRect

                                w : Int
                                w =
                                    Rect.width currentLocalRect

                                h : Int
                                h =
                                    Rect.height currentLocalRect

                                px : Int
                                px =
                                    Rect.posX parentRect

                                py : Int
                                py =
                                    Rect.posY parentRect

                                pw : Int
                                pw =
                                    Rect.width parentRect

                                ph : Int
                                ph =
                                    Rect.height parentRect

                                absX : Int
                                absX =
                                    x + px

                                absY : Int
                                absY =
                                    y + py

                                clippedX : Int
                                clippedX =
                                    clamp px (px + pw) absX

                                clippedY : Int
                                clippedY =
                                    clamp py (py + ph) absY

                                clippedW : Int
                                clippedW =
                                    Basics.max 0 (Basics.min w ((px + pw) - clippedX))

                                clippedH : Int
                                clippedH =
                                    Basics.max 0 (Basics.min h ((py + ph) - clippedY))
                            in
                            Rect.new ( clippedX, clippedY ) ( clippedW, clippedH )

                        Nothing ->
                            currentLocalRect

                updatedRects : Dict ObjectId Rect
                updatedRects =
                    Dict.insert currentId newAbsoluteRect currentAbsoluteRects

                children : List ObjectId
                children =
                    Dict.get currentId internals.childrenInDrawOrder
                        |> Maybe.withDefault []
            in
            List.foldl updateDescendants updatedRects children

        updatedAbsoluteRects : Dict ObjectId Rect
        updatedAbsoluteRects =
            updateDescendants objectId internals.absoluteRects
    in
    UI
        { internals
            | absoluteRects = updatedAbsoluteRects
        }


hitTest : Coordinate -> UI msg -> List String
hitTest coordinate ((UI internals) as ui) =
    Dict.keys internals.uiObjects
        |> List.filterMap
            (\objectId ->
                if Maybe.map (Rect.hitTest coordinate) (getAbsoluteRect ui objectId) == Just True then
                    Just objectId

                else
                    Nothing
            )


pickTopmostObject : List ObjectId -> UI msg -> Maybe ObjectId
pickTopmostObject candidates (UI internals) =
    let
        collectInDrawOrder : ObjectId -> List ObjectId
        collectInDrawOrder objectId =
            let
                children : List ObjectId
                children =
                    Dict.get objectId internals.childrenInDrawOrder
                        |> Maybe.withDefault []

                descendants : List ObjectId
                descendants =
                    List.concatMap collectInDrawOrder children
            in
            objectId :: descendants

        allInDrawOrder : List ObjectId
        allInDrawOrder =
            collectInDrawOrder domIds.root
    in
    allInDrawOrder
        |> List.filter (\id -> List.member id candidates)
        |> List.reverse
        |> List.head


createObject : Object msg -> UI msg -> UI msg
createObject object (UI internals) =
    let
        objectId : ObjectId
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
            | childrenInDrawOrder =
                case Dict.get params.parentId internals.childrenInDrawOrder of
                    Just _ ->
                        Dict.update params.parentId
                            (\maybeList ->
                                Maybe.map (\l -> l ++ [ params.objectId ]) maybeList
                            )
                            internals.childrenInDrawOrder

                    Nothing ->
                        Dict.insert params.parentId [ params.objectId ] internals.childrenInDrawOrder
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
                        internals.childrenInDrawOrder
            in
            UI { internals | childrenInDrawOrder = updatedDrawOrder }

        Nothing ->
            UI internals


updateObjectList : UI msg -> List ( ObjectId, Object msg -> Object msg ) -> UI msg
updateObjectList ui newPairs =
    List.foldl (\( key, updater ) -> updateObject key updater) ui newPairs


updateObject : ObjectId -> (Object msg -> Object msg) -> UI msg -> UI msg
updateObject objectId updateObj (UI internals) =
    UI
        { internals
            | uiObjects =
                Dict.update objectId
                    (\maybeObj -> Maybe.map updateObj maybeObj)
                    internals.uiObjects
        }
        |> updateAbsoluteRectsForDescendants objectId


view : { debugObject : String } -> UI msg -> Html msg
view params ui =
    let
        rootNotFoundView : Html msg
        rootNotFoundView =
            div [ class "UI.view couldn't find `root` object" ]
                []

        viewDebugRect : Html msg
        viewDebugRect =
            div [ id "DEBUGGING UI" ]
                [ getAbsoluteRect ui params.debugObject
                    |> Maybe.map (\rect -> MacOS.UI.View.Rectangle.view StyleDotted rect [])
                    |> Maybe.withDefault UIHelpers.none
                ]
    in
    div []
        [ getObject ui domIds.root
            |> Maybe.map (viewHelp params ui)
            |> Maybe.withDefault rootNotFoundView
        , viewDebugRect
        ]


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
    Dict.get parentId internals.childrenInDrawOrder
        |> Maybe.withDefault []
