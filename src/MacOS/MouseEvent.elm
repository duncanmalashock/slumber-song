module MacOS.MouseEvent exposing (MouseEvent(..), filterByObjectId)


type MouseEvent
    = MouseDown String
    | MouseUp String
    | Click String
    | DoubleClick String
    | DragStart String


filterByObjectId : String -> List MouseEvent -> List MouseEvent
filterByObjectId objectId events =
    events
        |> List.filter
            (\e ->
                case e of
                    MouseDown idToTest ->
                        idToTest == objectId

                    MouseUp idToTest ->
                        True

                    Click idToTest ->
                        idToTest == objectId

                    DoubleClick idToTest ->
                        idToTest == objectId

                    DragStart idToTest ->
                        idToTest == objectId
            )
