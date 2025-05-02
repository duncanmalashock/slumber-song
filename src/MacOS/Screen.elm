module MacOS.Screen exposing (Screen, height, logical, new, scaleAttr, toScreenCoordinates, update, width)

import Html
import Html.Attributes
import MacOS.Coordinate as Coordinate exposing (Coordinate)
import MacOS.Rect as Rect exposing (Rect)


type Screen
    = Screen Internals


type Scale
    = ShrinkToWidth Int
    | Scale1
    | Scale1_5
    | Scale2


type alias Internals =
    { logical : Rect
    , scale : Scale
    , positionInBrowser : Coordinate
    , devicePixelRatio : Float
    , browserWidth : Int
    }


new :
    { screenInPixels : ( Int, Int )
    , browser : { x : Int, y : Int }
    , devicePixelRatio : Float
    }
    -> Screen
new params =
    let
        newLogical : Rect
        newLogical =
            Rect.new
                ( 0, 0 )
                ( Tuple.first params.screenInPixels
                , Tuple.second params.screenInPixels
                )

        selectedScale : Scale
        selectedScale =
            largestScaleAvailable
                { screen = newLogical
                , browser = params.browser
                }

        centeredPosition : Coordinate
        centeredPosition =
            calculatePositionInBrowser
                { scale = selectedScale
                , logical = newLogical
                , browser = params.browser
                , devicePixelRatio = params.devicePixelRatio
                }
    in
    Screen
        { logical = newLogical
        , scale = selectedScale
        , positionInBrowser = centeredPosition
        , devicePixelRatio = params.devicePixelRatio
        , browserWidth = params.browser.x
        }


update : { x : Int, y : Int } -> Screen -> Screen
update browser (Screen internals) =
    let
        newScale : Scale
        newScale =
            largestScaleAvailable
                { screen = internals.logical
                , browser = browser
                }

        newPosition : Coordinate
        newPosition =
            calculatePositionInBrowser
                { logical = internals.logical
                , scale = newScale
                , browser = browser
                , devicePixelRatio = internals.devicePixelRatio
                }
    in
    Screen
        { internals
            | scale = newScale
            , positionInBrowser = newPosition
        }


toScreenCoordinates : Screen -> Coordinate -> Coordinate
toScreenCoordinates ((Screen internals) as screen) coord =
    let
        scaleFactor : Float
        scaleFactor =
            scale screen

        coordX : Float
        coordX =
            toFloat (Coordinate.x coord)

        coordY : Float
        coordY =
            toFloat (Coordinate.y coord)

        offsetX : Float
        offsetX =
            toFloat (Coordinate.x internals.positionInBrowser)

        offsetY : Float
        offsetY =
            toFloat (Coordinate.x internals.positionInBrowser)
    in
    Coordinate.new
        ( round ((coordX - offsetX) / scaleFactor)
        , round ((coordY - offsetY) / scaleFactor)
        )


largestScaleAvailable : { screen : Rect, browser : { x : Int, y : Int } } -> Scale
largestScaleAvailable params =
    let
        fits : Float -> Bool
        fits scaleFactor =
            let
                widthIfScaled : Float
                widthIfScaled =
                    toFloat (Rect.width params.screen) * scaleFactor

                heightIfScaled : Float
                heightIfScaled =
                    toFloat (Rect.height params.screen) * scaleFactor

                browserWidth : Float
                browserWidth =
                    toFloat params.browser.x

                browserHeight : Float
                browserHeight =
                    toFloat params.browser.y
            in
            (widthIfScaled <= browserWidth)
                && (heightIfScaled <= browserHeight)
    in
    if fits 2 then
        Scale2

    else if fits 1.5 then
        Scale1_5

    else if fits 1 then
        Scale1

    else
        ShrinkToWidth params.browser.x


calculatePositionInBrowser :
    { scale : Scale
    , logical : Rect
    , browser : { x : Int, y : Int }
    , devicePixelRatio : Float
    }
    -> Coordinate
calculatePositionInBrowser params =
    let
        scaleFactor : Float
        scaleFactor =
            scaleToFloat (Rect.width params.logical) params.browser.x params.scale

        scaledScreenWidth : Float
        scaledScreenWidth =
            toFloat (Rect.width params.logical) * scaleFactor

        scaledScreenHeight : Float
        scaledScreenHeight =
            toFloat (Rect.height params.logical) * scaleFactor

        offsetX : Float
        offsetX =
            (toFloat params.browser.x - scaledScreenWidth) / 2

        offsetY : Float
        offsetY =
            (toFloat params.browser.y - scaledScreenHeight) / 2
    in
    Coordinate.new ( round offsetX, round offsetY )


positionInBrowser : Screen -> Coordinate
positionInBrowser (Screen internals) =
    internals.positionInBrowser


logical : Screen -> Rect
logical (Screen internals) =
    internals.logical


width : Screen -> Int
width (Screen internals) =
    Rect.width internals.logical


height : Screen -> Int
height (Screen internals) =
    Rect.height internals.logical


scale : Screen -> Float
scale (Screen internals) =
    scaleToFloat
        (Rect.width internals.logical)
        internals.browserWidth
        internals.scale


scaleAttr : Screen -> Html.Attribute msg
scaleAttr screen =
    Html.Attributes.style "transform"
        ("scale(" ++ String.fromFloat (scale screen) ++ ")")


scaleToFloat : Int -> Int -> Scale -> Float
scaleToFloat logicalWidth browserWidth scaleValue =
    case scaleValue of
        ShrinkToWidth targetWidth ->
            toFloat targetWidth / toFloat logicalWidth

        Scale1 ->
            1.0

        Scale1_5 ->
            1.5

        Scale2 ->
            2.0
