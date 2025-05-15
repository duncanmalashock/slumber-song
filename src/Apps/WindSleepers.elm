module Apps.WindSleepers exposing (Model, Msg(..), init, update)

import MacOS.Instruction as Instruction exposing (Instruction)
import MacOS.Rect as Rect
import MacOS.ToAppMsg as ToAppMsg exposing (ToAppMsg(..))
import MacOS.UI.Helpers as UIHelpers exposing (domIds)
import MacOS.UI.Object as UIObject
import MacOS.UI.View as View
import MacOS.UI.View.Window as Window


type alias Model =
    {}


init : ( Model, List (Instruction msg) )
init =
    ( {}
    , [ Instruction.CreateWindow
            { withId = "scene"
            , window =
                { title = "Temple Ruins"
                , closeMsg = Nothing
                }
            , rect = Rect.new ( 128, 64 ) ( 256, 172 )
            }
      , Instruction.CreateWindow
            { withId = "inventory"
            , window =
                { title = "inventory"
                , closeMsg = Nothing
                }
            , rect = Rect.new ( 4, 28 ) ( 118, 225 )
            }
      , Instruction.CreateObject
            { object =
                UIObject.image
                    { id = "scene:ruins"
                    , url = "WindSleepers/ruins.gif"
                    , size = ( 256, 172 )
                    }
            }
      , Instruction.AttachObject
            { objectId = "scene:ruins"
            , parentId = "scene"
            , rect = Rect.new ( 0, 0 ) ( 256, 172 )
            }
      , Instruction.CreateObject
            { object =
                UIObject.image
                    { id = "obj:skull"
                    , url = "WindSleepers/skull.gif"
                    , size = ( 15, 17 )
                    }
                    |> UIObject.setDragOptions
                        { traveling =
                            View.image
                                { url = "WindSleepers/skull.gif"
                                , size = ( 15, 17 )
                                }
                        }
            }
      , Instruction.AttachObject
            { objectId = "obj:skull"
            , parentId = "inventory"
            , rect = Rect.new ( 6, 29 ) ( 15, 17 )
            }
      ]
    )


type Msg
    = ReceivedMsgFromOS ToAppMsg


update : Msg -> Model -> ( Model, List (Instruction msg) )
update msg model =
    case msg of
        ReceivedMsgFromOS toAppMsg ->
            case toAppMsg of
                DroppedObject droppedObjectInfo ->
                    if droppedObjectInfo.isWindow then
                        ( model
                        , []
                        )

                    else if droppedObjectInfo.droppedOnWindow == Just "scene" then
                        ( model
                        , []
                        )

                    else if droppedObjectInfo.droppedOnWindow == Just "inventory" then
                        ( model
                        , []
                        )

                    else if droppedObjectInfo.droppedOnWindow == Nothing then
                        ( model
                        , []
                        )

                    else
                        ( model
                        , []
                        )
