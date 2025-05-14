module Vent.Interaction exposing (Interaction(..), detect, handlesCommand, handlesObject)

import Vent.Command exposing (Command(..))


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


handlesCommand : Command -> Interaction -> Bool
handlesCommand command interaction =
    case interaction of
        AttemptExamine { objectId } ->
            command == Examine

        AttemptOpen { objectId } ->
            command == Open

        AttemptClose { objectId } ->
            command == Close

        AttemptSpeak { objectId } ->
            command == Speak

        AttemptOperate { sourceObjectId, targetObjectId } ->
            command == Operate

        AttemptGo { objectId } ->
            command == Go

        AttemptHit { objectId } ->
            command == Hit

        AttemptConsume { objectId } ->
            command == Consume

        AttemptMoveObject { objectId } ->
            command == Move


handlesObject : String -> Interaction -> Bool
handlesObject objectIdToMatch interaction =
    case interaction of
        AttemptExamine { objectId } ->
            objectId == objectIdToMatch

        AttemptOpen { objectId } ->
            objectId == objectIdToMatch

        AttemptClose { objectId } ->
            objectId == objectIdToMatch

        AttemptSpeak { objectId } ->
            objectId == objectIdToMatch

        AttemptOperate { sourceObjectId, targetObjectId } ->
            (sourceObjectId == objectIdToMatch) || (targetObjectId == objectIdToMatch)

        AttemptGo { objectId } ->
            objectId == objectIdToMatch

        AttemptHit { objectId } ->
            objectId == objectIdToMatch

        AttemptConsume { objectId } ->
            objectId == objectIdToMatch

        AttemptMoveObject { objectId } ->
            objectId == objectIdToMatch


detect :
    { selectedCommand : Maybe Command
    , sourceObjectId : Maybe String
    , targetObjectId : Maybe String
    }
    -> Maybe Interaction
detect params =
    case params.selectedCommand of
        Nothing ->
            Nothing

        Just Move ->
            -- Can't be selected with the menu
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
