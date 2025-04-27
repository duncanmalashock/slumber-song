module Vent.Canonicalize exposing (Error(..), execute)

import Attribute exposing (Attribute(..))
import Command exposing (Command)
import Effect exposing (Effect)
import Expression exposing (ExpressionBool(..))
import Interaction exposing (Interaction(..))
import ObjectStore exposing (ObjectStore)
import Script exposing (Script)
import Trigger exposing (Trigger)
import Update exposing (Update)
import Vent.Parse


type Error
    = ExpectedCondition
    | ExpressionError ExpressionError
    | ExpectedUpdates UpdatesError


execute : String -> ObjectStore -> Vent.Parse.Script -> Result Error Script
execute localObject objectStore script =
    script
        |> convertTrigger
        |> Result.andThen (convertCondition localObject objectStore)
        |> Result.andThen (convertResults localObject objectStore)


convertTrigger :
    Vent.Parse.Script
    ->
        Result
            Error
            { trigger : Trigger
            , statements : List Vent.Parse.Statement
            }
convertTrigger (Vent.Parse.Script trigger statements) =
    let
        convertedTrigger =
            case trigger of
                Vent.Parse.OnAny ->
                    Trigger.OnAny

                Vent.Parse.OnExamine ->
                    Trigger.OnCommand Command.Examine

                Vent.Parse.OnOpen ->
                    Trigger.OnCommand Command.Open

                Vent.Parse.OnClose ->
                    Trigger.OnCommand Command.Close

                Vent.Parse.OnSpeak ->
                    Trigger.OnCommand Command.Speak

                Vent.Parse.OnOperate ->
                    Trigger.OnCommand Command.Operate

                Vent.Parse.OnGo ->
                    Trigger.OnCommand Command.Go

                Vent.Parse.OnHit ->
                    Trigger.OnCommand Command.Hit

                Vent.Parse.OnConsume ->
                    Trigger.OnCommand Command.Consume
    in
    Ok
        { trigger = convertedTrigger
        , statements = statements
        }


convertCondition : String -> ObjectStore -> { trigger : Trigger, statements : List Vent.Parse.Statement } -> Result Error { trigger : Trigger, condition : ExpressionBool, statements : List Vent.Parse.Statement }
convertCondition localObject objectStore { trigger, statements } =
    case statements of
        (Vent.Parse.IfThen expr thenStatements) :: rest ->
            case validateBoolExpr localObject objectStore expr of
                Ok validCondition ->
                    Ok
                        { trigger = trigger
                        , condition = validCondition
                        , statements = rest ++ statements
                        }

                Err error ->
                    Err (ExpressionError error)

        _ ->
            Err ExpectedCondition


type ExpressionError
    = ReferenceToAttributeNotFound String String
    | ReferenceToObjectNotFound String
    | AttributeTypeError AttributeTypeError
    | ExpressionComparisonTodo


type AttributeTypeError
    = AttributeIsOfTypeInt String String
    | AttributeIsOfTypeString String String
    | AttributeIsOfTypeBool String String


type RefType
    = RefBool
    | RefInt
    | RefString


validateBoolExpr : String -> ObjectStore -> Vent.Parse.Expr -> Result ExpressionError ExpressionBool
validateBoolExpr localObject objectStore expr =
    case expr of
        Vent.Parse.LiteralBool bool ->
            Expression.LiteralBool bool
                |> Ok

        Vent.Parse.Ref (Vent.Parse.Local attrKey) ->
            case resolveReference objectStore localObject attrKey of
                Ok RefBool ->
                    Ok (ExpAttributeBool { objId = localObject, key = attrKey })

                Ok RefInt ->
                    Err (AttributeTypeError (AttributeIsOfTypeInt localObject attrKey))

                Ok RefString ->
                    Err (AttributeTypeError (AttributeIsOfTypeString localObject attrKey))

                Err err ->
                    Err err

        Vent.Parse.Ref (Vent.Parse.Field objKey attrKey) ->
            case resolveReference objectStore objKey attrKey of
                Ok RefBool ->
                    Ok (ExpAttributeBool { objId = objKey, key = attrKey })

                Ok RefInt ->
                    Err (AttributeTypeError (AttributeIsOfTypeInt localObject attrKey))

                Ok RefString ->
                    Err (AttributeTypeError (AttributeIsOfTypeString localObject attrKey))

                Err err ->
                    Err err

        Vent.Parse.Comparison leftSide operator rightSide ->
            -- case operator of
            --     Parse.EqualTo ->
            --     Parse.LessThan ->
            --     Parse.GreaterThan ->
            --     Parse.Contains ->
            Err ExpressionComparisonTodo


resolveReference : ObjectStore -> String -> String -> Result ExpressionError RefType
resolveReference objectStore obj attr =
    if ObjectStore.idExists obj objectStore then
        case
            ObjectStore.getAttribute
                objectStore
                { objectId = obj
                , attributeId = attr
                }
        of
            Just foundAttr ->
                case foundAttr of
                    Attribute.AttributeBool bool ->
                        Ok RefBool

                    Attribute.AttributeInt int ->
                        Ok RefInt

                    Attribute.AttributeString str ->
                        Ok RefString

            Nothing ->
                Err (ReferenceToAttributeNotFound obj attr)

    else
        Err (ReferenceToObjectNotFound obj)


convertResults : String -> ObjectStore -> { trigger : Trigger, condition : ExpressionBool, statements : List Vent.Parse.Statement } -> Result Error Script
convertResults localObject objectStore { trigger, condition, statements } =
    Err (ExpectedUpdates UpdatesToDo)


type UpdatesError
    = UpdatesToDo
