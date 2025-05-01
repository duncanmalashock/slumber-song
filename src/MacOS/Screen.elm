module MacOS.Screen exposing (Screen, height, logical, new, scale, scaleAttr, toScreenCoordinates, update, width)

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
    | Scale2_5


type alias Internals =
    { logical : Rect
    , scale : Scale
    , positionInBrowser : Coordinate
    , devicePixelRatio : Float
    , browserWidth : Int
    }


new : { screen : Coordinate, browser : { x : Int, y : Int }, devicePixelRatio : Float } -> Screen
new params =
    let
        selectedScale : Scale
        selectedScale =
            largestScaleAvailable params

        centeredPosition : Coordinate
        centeredPosition =
            calculatePositionInBrowser (Coordinate.x params.screen) selectedScale params
    in
    Screen
        { logical =
            Rect.new
                ( 0, 0 )
                ( Coordinate.x params.screen, Coordinate.y params.screen )
        , scale = selectedScale
        , positionInBrowser = centeredPosition
        , devicePixelRatio = params.devicePixelRatio
        , browserWidth = params.browser.x
        }


update : { x : Int, y : Int } -> Screen -> Screen
update newBrowserSize (Screen internals) =
    let
        screenSize =
            Coordinate.new ( Rect.width internals.logical, Rect.height internals.logical )

        params =
            { screen = screenSize, browser = newBrowserSize, devicePixelRatio = internals.devicePixelRatio }

        newScale =
            largestScaleAvailable params

        newPosition =
            calculatePositionInBrowser (Rect.width internals.logical) newScale params
    in
    Screen
        { internals
            | scale = newScale
            , positionInBrowser = newPosition
        }


toScreenCoordinates : Screen -> Coordinate -> Coordinate
toScreenCoordinates (Screen internals) coord =
    let
        scaleFactor : Float
        scaleFactor =
            scaleToFloat (Rect.width internals.logical) internals.browserWidth internals.scale

        browserPos =
            internals.positionInBrowser
    in
    Coordinate.new
        ( round ((toFloat (Coordinate.x coord) - toFloat (Coordinate.x browserPos)) / scaleFactor)
        , round ((toFloat (Coordinate.y coord) - toFloat (Coordinate.y browserPos)) / scaleFactor)
        )


largestScaleAvailable : { screen : Coordinate, browser : { x : Int, y : Int }, devicePixelRatio : Float } -> Scale
largestScaleAvailable params =
    let
        fits : Float -> Bool
        fits scaleFactor =
            toFloat (Coordinate.x params.screen)
                * scaleFactor
                <= toFloat params.browser.x
                && toFloat (Coordinate.y params.screen)
                * scaleFactor
                <= toFloat params.browser.y
    in
    if fits 2.5 then
        Scale2_5

    else if fits 2 then
        Scale2

    else if fits 1.5 then
        Scale1_5

    else if fits 1 then
        Scale1

    else
        ShrinkToWidth params.browser.x


calculatePositionInBrowser : Int -> Scale -> { screen : Coordinate, browser : { x : Int, y : Int }, devicePixelRatio : Float } -> Coordinate
calculatePositionInBrowser logicalWidth scaleValue params =
    let
        scaleFactor : Float
        scaleFactor =
            scaleToFloat logicalWidth params.browser.x scaleValue

        scaledScreenWidth =
            toFloat (Coordinate.x params.screen) * scaleFactor

        scaledScreenHeight =
            toFloat (Coordinate.y params.screen) * scaleFactor

        offsetX =
            (toFloat params.browser.x - scaledScreenWidth) / 2

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
    scaleToFloat (Rect.width internals.logical) internals.browserWidth internals.scale


scaleAttr : Screen -> Html.Attribute msg
scaleAttr (Screen internals) =
    Html.Attributes.style "transform"
        ("scale(" ++ String.fromFloat (scaleToFloat (Rect.width internals.logical) internals.browserWidth internals.scale) ++ ")")


scaleToFloat : Int -> Int -> Scale -> Float
scaleToFloat logicalWidth browserWidth s =
    case s of
        ShrinkToWidth targetWidth ->
            toFloat targetWidth / toFloat logicalWidth

        Scale1 ->
            1.0

        Scale1_5 ->
            1.5

        Scale2 ->
            2.0

        Scale2_5 ->
            2.5
