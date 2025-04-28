module Vent.Canonicalize exposing (Error(..), execute)

import Attribute exposing (Attribute(..))
import Command exposing (Command)
import Effect exposing (Effect)
import Expression exposing (ExpressionBool(..))
import Interaction exposing (Interaction(..))
import ObjectStore exposing (ObjectStore)
import Result.Extra as Result
import Script exposing (Script)
import Trigger exposing (Trigger)
import Update exposing (Update)
import Vent.Parse


type Error
    = ExpectedCondition
    | ExpressionError ExpressionError
    | StatementError StatementError


execute : String -> ObjectStore -> Vent.Parse.Script -> Result Error Script
execute localObject objectStore script =
    script
        |> convertTrigger
        |> Result.andThen (convertCondition localObject objectStore)
        |> convertResults localObject objectStore


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
    | TypeError TypeError
    | ComparisonTypeMismatch (Result ExpressionError ExprType) Vent.Parse.Expr


type TypeError
    = AttributeIsOfTypeInt String String
    | AttributeIsOfTypeString String String
    | AttributeIsOfTypeBool String String
    | LiteralIsOfTypeInt Int
    | LiteralIsOfTypeString String
    | LiteralIsOfTypeBool Bool
    | ComparisonIsOfTypeBool
    | InvalidOperatorForType ExprType Vent.Parse.Operator


type RefType
    = RefBool
    | RefInt
    | RefString


type ExprType
    = ExprBool
    | ExprInt
    | ExprString


checkExprType : String -> ObjectStore -> Vent.Parse.Expr -> Result ExpressionError ExprType
checkExprType localObject objectStore expr =
    case expr of
        Vent.Parse.Comparison leftSide _ rightSide ->
            Ok ExprBool

        Vent.Parse.Ref var ->
            let
                ( objKey, attrKey ) =
                    case var of
                        Vent.Parse.Local a ->
                            ( localObject, a )

                        Vent.Parse.Field o a ->
                            ( o, a )
            in
            case resolveReference objectStore objKey attrKey of
                Ok RefBool ->
                    Ok ExprBool

                Ok RefInt ->
                    Ok ExprInt

                Ok RefString ->
                    Ok ExprString

                Err err ->
                    Err err

        Vent.Parse.LiteralBool _ ->
            Ok ExprBool

        Vent.Parse.LiteralString _ ->
            Ok ExprString

        Vent.Parse.LiteralInt _ ->
            Ok ExprInt


validateIntExpr : String -> ObjectStore -> Vent.Parse.Expr -> Result ExpressionError Expression.ExpressionInt
validateIntExpr localObject objectStore expr =
    case expr of
        Vent.Parse.LiteralInt int ->
            Expression.LiteralInt int
                |> Ok

        Vent.Parse.LiteralString string ->
            Err (TypeError (LiteralIsOfTypeString string))

        Vent.Parse.LiteralBool bool ->
            Err (TypeError (LiteralIsOfTypeBool bool))

        Vent.Parse.Comparison _ _ _ ->
            Err (TypeError ComparisonIsOfTypeBool)

        Vent.Parse.Ref (Vent.Parse.Local attrKey) ->
            case resolveReference objectStore localObject attrKey of
                Ok RefInt ->
                    Ok (Expression.ExpAttributeInt { objId = localObject, key = attrKey })

                Ok RefBool ->
                    Err (TypeError (AttributeIsOfTypeBool localObject attrKey))

                Ok RefString ->
                    Err (TypeError (AttributeIsOfTypeString localObject attrKey))

                Err err ->
                    Err err

        Vent.Parse.Ref (Vent.Parse.Field objKey attrKey) ->
            case resolveReference objectStore objKey attrKey of
                Ok RefInt ->
                    Ok (Expression.ExpAttributeInt { objId = objKey, key = attrKey })

                Ok RefBool ->
                    Err (TypeError (AttributeIsOfTypeBool localObject attrKey))

                Ok RefString ->
                    Err (TypeError (AttributeIsOfTypeString localObject attrKey))

                Err err ->
                    Err err


validateStringExpr : String -> ObjectStore -> Vent.Parse.Expr -> Result ExpressionError Expression.ExpressionString
validateStringExpr localObject objectStore expr =
    case expr of
        Vent.Parse.LiteralString int ->
            Expression.LiteralString int
                |> Ok

        Vent.Parse.LiteralInt int ->
            Err (TypeError (LiteralIsOfTypeInt int))

        Vent.Parse.LiteralBool bool ->
            Err (TypeError (LiteralIsOfTypeBool bool))

        Vent.Parse.Comparison _ _ _ ->
            Err (TypeError ComparisonIsOfTypeBool)

        Vent.Parse.Ref (Vent.Parse.Local attrKey) ->
            case resolveReference objectStore localObject attrKey of
                Ok RefString ->
                    Ok (Expression.ExpAttributeString { objId = localObject, key = attrKey })

                Ok RefBool ->
                    Err (TypeError (AttributeIsOfTypeBool localObject attrKey))

                Ok RefInt ->
                    Err (TypeError (AttributeIsOfTypeInt localObject attrKey))

                Err err ->
                    Err err

        Vent.Parse.Ref (Vent.Parse.Field objKey attrKey) ->
            case resolveReference objectStore objKey attrKey of
                Ok RefString ->
                    Ok (Expression.ExpAttributeString { objId = objKey, key = attrKey })

                Ok RefBool ->
                    Err (TypeError (AttributeIsOfTypeBool localObject attrKey))

                Ok RefInt ->
                    Err (TypeError (AttributeIsOfTypeInt localObject attrKey))

                Err err ->
                    Err err


validateBoolExpr : String -> ObjectStore -> Vent.Parse.Expr -> Result ExpressionError ExpressionBool
validateBoolExpr localObject objectStore expr =
    case expr of
        Vent.Parse.LiteralBool bool ->
            Expression.LiteralBool bool
                |> Ok

        Vent.Parse.LiteralString string ->
            Err (TypeError (LiteralIsOfTypeString string))

        Vent.Parse.LiteralInt int ->
            Err (TypeError (LiteralIsOfTypeInt int))

        Vent.Parse.Ref (Vent.Parse.Local attrKey) ->
            case resolveReference objectStore localObject attrKey of
                Ok RefBool ->
                    Ok (ExpAttributeBool { objId = localObject, key = attrKey })

                Ok RefInt ->
                    Err (TypeError (AttributeIsOfTypeInt localObject attrKey))

                Ok RefString ->
                    Err (TypeError (AttributeIsOfTypeString localObject attrKey))

                Err err ->
                    Err err

        Vent.Parse.Ref (Vent.Parse.Field objKey attrKey) ->
            case resolveReference objectStore objKey attrKey of
                Ok RefBool ->
                    Ok (ExpAttributeBool { objId = objKey, key = attrKey })

                Ok RefInt ->
                    Err (TypeError (AttributeIsOfTypeInt localObject attrKey))

                Ok RefString ->
                    Err (TypeError (AttributeIsOfTypeString localObject attrKey))

                Err err ->
                    Err err

        Vent.Parse.Comparison leftSide operator rightSide ->
            let
                ( leftType, rightType ) =
                    ( checkExprType localObject objectStore leftSide
                    , checkExprType localObject objectStore rightSide
                    )
            in
            case leftType of
                Ok ExprBool ->
                    case
                        ( validateBoolExpr localObject objectStore leftSide
                        , validateBoolExpr localObject objectStore rightSide
                        )
                    of
                        ( Ok validatedLeft, Ok validatedRight ) ->
                            case operator of
                                Vent.Parse.EqualTo ->
                                    Ok (Expression.BoolEquals validatedLeft validatedRight)

                                Vent.Parse.LessThan ->
                                    Err (TypeError (InvalidOperatorForType ExprBool Vent.Parse.LessThan))

                                Vent.Parse.GreaterThan ->
                                    Err (TypeError (InvalidOperatorForType ExprBool Vent.Parse.GreaterThan))

                                Vent.Parse.Contains ->
                                    Err (TypeError (InvalidOperatorForType ExprBool Vent.Parse.Contains))

                        _ ->
                            Err (ComparisonTypeMismatch leftType rightSide)

                Ok ExprInt ->
                    case
                        ( validateIntExpr localObject objectStore leftSide
                        , validateIntExpr localObject objectStore rightSide
                        )
                    of
                        ( Ok validatedLeft, Ok validatedRight ) ->
                            case operator of
                                Vent.Parse.EqualTo ->
                                    Ok (Expression.IntEquals validatedLeft validatedRight)

                                Vent.Parse.LessThan ->
                                    Ok (Expression.IntLessThan validatedLeft validatedRight)

                                Vent.Parse.GreaterThan ->
                                    Ok (Expression.IntGreaterThan validatedLeft validatedRight)

                                Vent.Parse.Contains ->
                                    Err (TypeError (InvalidOperatorForType ExprInt Vent.Parse.Contains))

                        _ ->
                            Err (ComparisonTypeMismatch leftType rightSide)

                Ok ExprString ->
                    case
                        ( validateStringExpr localObject objectStore leftSide
                        , validateStringExpr localObject objectStore rightSide
                        )
                    of
                        ( Ok validatedLeft, Ok validatedRight ) ->
                            case operator of
                                Vent.Parse.EqualTo ->
                                    Ok (Expression.StringEquals validatedLeft validatedRight)

                                Vent.Parse.LessThan ->
                                    Err (TypeError (InvalidOperatorForType ExprString Vent.Parse.LessThan))

                                Vent.Parse.GreaterThan ->
                                    Err (TypeError (InvalidOperatorForType ExprString Vent.Parse.GreaterThan))

                                Vent.Parse.Contains ->
                                    Ok (Expression.StringContains validatedLeft validatedRight)

                        _ ->
                            Err (ComparisonTypeMismatch leftType rightSide)

                Err err ->
                    Err err


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


convertResults : String -> ObjectStore -> Result Error { trigger : Trigger, condition : ExpressionBool, statements : List Vent.Parse.Statement } -> Result Error Script
convertResults localObject objectStore result =
    case result of
        Ok { trigger, condition, statements } ->
            statements
                |> List.concatMap unwrapStatements
                |> List.foldl
                    (convertResultStatement localObject objectStore)
                    (Ok { trigger = trigger, condition = condition, updates = [], effects = [] })

        Err err ->
            Err err


unwrapStatements : Vent.Parse.Statement -> List Vent.Parse.Statement
unwrapStatements statement =
    case statement of
        Vent.Parse.IfThen expr statements ->
            statements

        Vent.Parse.Assignment variable expr ->
            [ Vent.Parse.Assignment variable expr ]

        Vent.Parse.Efct effect ->
            [ Vent.Parse.Efct effect ]


convertResultStatement : String -> ObjectStore -> Vent.Parse.Statement -> Result Error Script -> Result Error Script
convertResultStatement localObject objectStore statement current =
    case current of
        Ok script ->
            case statement of
                Vent.Parse.IfThen expr statements ->
                    Err (StatementError (UnexpectedIfThenStatement (Vent.Parse.IfThen expr statements)))

                Vent.Parse.Assignment variable expr ->
                    case variable of
                        Vent.Parse.Local attrKey ->
                            updateFromReference script expr objectStore localObject attrKey

                        Vent.Parse.Field objKey attrKey ->
                            updateFromReference script expr objectStore objKey attrKey

                Vent.Parse.Efct effect ->
                    case effect of
                        Vent.Parse.PrintText string ->
                            Ok
                                { script
                                    | effects =
                                        script.effects
                                            ++ [ Effect.PrintText string
                                               ]
                                }

        Err err ->
            Err err


type StatementError
    = UnexpectedIfThenStatement Vent.Parse.Statement


updateFromReference : Script -> Vent.Parse.Expr -> ObjectStore -> String -> String -> Result Error Script
updateFromReference script expr objectStore localObject attrKey =
    case resolveReference objectStore localObject attrKey of
        Ok RefBool ->
            case validateBoolExpr localObject objectStore expr of
                Ok expression ->
                    Ok
                        { script
                            | updates =
                                script.updates
                                    ++ [ Update.SetBoolAttribute { objId = localObject, attributeKey = attrKey, value = expression }
                                       ]
                        }

                Err err ->
                    Err (ExpressionError err)

        Ok RefInt ->
            case validateIntExpr localObject objectStore expr of
                Ok expression ->
                    Ok
                        { script
                            | updates =
                                script.updates
                                    ++ [ Update.SetIntAttribute { objId = localObject, attributeKey = attrKey, value = expression }
                                       ]
                        }

                Err err ->
                    Err (ExpressionError err)

        Ok RefString ->
            case validateStringExpr localObject objectStore expr of
                Ok expression ->
                    Ok
                        { script
                            | updates =
                                script.updates
                                    ++ [ Update.SetStringAttribute { objId = localObject, attributeKey = attrKey, value = expression }
                                       ]
                        }

                Err err ->
                    Err (ExpressionError err)

        Err err ->
            Err (ExpressionError err)
