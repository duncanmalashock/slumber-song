module Interaction exposing (Interaction(..), detect, matchesCommand)

import Command exposing (Command(..))


type Interaction
    = AttemptMoveObject { objectId : String, from : ObjectLocation, to : ObjectLocation }
    | AttemptExamine { objectId : String }
    | AttemptOpen { objectId : String }
    | AttemptClose { objectId : String }
    | AttemptSpeak { objectId : String }
    | AttemptOperate { sourceObjectId : String, targetObjectId : String }
    | AttemptGo { objectId : String }
    | AttemptHit { objectId : String }
    | AttemptConsume { objectId : String }


type alias ObjectLocation =
    { windowObjectId : String
    , xPos : Int
    , yPos : Int
    }


matchesCommand : Command -> String -> Interaction -> Bool
matchesCommand command objectIdToMatch interaction =
    case interaction of
        AttemptExamine { objectId } ->
            command == Examine && (objectId == objectIdToMatch)

        AttemptOpen { objectId } ->
            command == Open && (objectId == objectIdToMatch)

        AttemptClose { objectId } ->
            command == Close && (objectId == objectIdToMatch)

        AttemptSpeak { objectId } ->
            command == Speak && (objectId == objectIdToMatch)

        AttemptOperate { sourceObjectId, targetObjectId } ->
            command == Operate && ((sourceObjectId == objectIdToMatch) || (targetObjectId == objectIdToMatch))

        AttemptGo { objectId } ->
            command == Go && (objectId == objectIdToMatch)

        AttemptHit { objectId } ->
            command == Hit && (objectId == objectIdToMatch)

        AttemptConsume { objectId } ->
            command == Consume && (objectId == objectIdToMatch)

        AttemptMoveObject _ ->
            False


detect :
    { selectedCommand : Maybe Command
    , sourceObjectId : Maybe String
    , targetObjectId : Maybe String
    , objectDragInfo : Maybe { objectId : String, from : ObjectLocation, to : ObjectLocation }
    }
    -> Maybe Interaction
detect params =
    case params.selectedCommand of
        Nothing ->
            case params.objectDragInfo of
                Just { objectId, from, to } ->
                    Just <|
                        AttemptMoveObject
                            { objectId = objectId
                            , from = from
                            , to = to
                            }

                Nothing ->
                    Nothing

        Just Examine ->
            case params.sourceObjectId of
                Just obj ->
                    Just <|
                        AttemptExamine { objectId = obj }

                Nothing ->
                    Nothing

        Just Open ->
            case params.sourceObjectId of
                Just obj ->
                    Just <|
                        AttemptOpen { objectId = obj }

                Nothing ->
                    Nothing

        Just Close ->
            case params.sourceObjectId of
                Just obj ->
                    Just <|
                        AttemptClose { objectId = obj }

                Nothing ->
                    Nothing

        Just Speak ->
            case params.sourceObjectId of
                Just obj ->
                    Just <|
                        AttemptSpeak { objectId = obj }

                Nothing ->
                    Nothing

        Just Operate ->
            case ( params.sourceObjectId, params.targetObjectId ) of
                ( Just sourceObj, Just targetObj ) ->
                    Just <|
                        AttemptOperate { sourceObjectId = sourceObj, targetObjectId = targetObj }

                ( Nothing, Nothing ) ->
                    Nothing

                ( Just _, Nothing ) ->
                    Nothing

                ( Nothing, Just _ ) ->
                    Nothing

        Just Go ->
            case params.sourceObjectId of
                Just obj ->
                    Just <|
                        AttemptGo { objectId = obj }

                Nothing ->
                    Nothing

        Just Hit ->
            case params.sourceObjectId of
                Just obj ->
                    Just <|
                        AttemptHit { objectId = obj }

                Nothing ->
                    Nothing

        Just Consume ->
            case params.sourceObjectId of
                Just obj ->
                    Just <|
                        AttemptConsume { objectId = obj }

                Nothing ->
                    Nothing
