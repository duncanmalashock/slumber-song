module Vent.VentScript.Canonicalize exposing (Error(..), execute)

import Result.Extra as Result
import Vent.Attribute as Attribute exposing (Attribute(..))
import Vent.Command as Command exposing (Command)
import Vent.Effect as Effect exposing (Effect)
import Vent.Expression as Expression exposing (ExpressionBool(..))
import Vent.Interaction exposing (Interaction(..))
import Vent.ObjectStore as ObjectStore exposing (ObjectStore)
import Vent.Script exposing (Script)
import Vent.Trigger as Trigger exposing (Trigger)
import Vent.Update as Update exposing (Update)
import Vent.VentScript.Parse as Parse


type Error
    = ExpectedCondition
    | ExpressionError ExpressionError
    | StatementError StatementError


execute : String -> ObjectStore -> Parse.Script -> Result Error Script
execute localObject objectStore script =
    script
        |> convertTrigger
        |> Result.andThen (convertCondition localObject objectStore)
        |> convertResults localObject objectStore


convertTrigger :
    Parse.Script
    ->
        Result
            Error
            { trigger : Trigger
            , statements : List Parse.Statement
            }
convertTrigger (Parse.Script trigger statements) =
    let
        convertedTrigger =
            case trigger of
                Parse.OnAny ->
                    Trigger.OnAny

                Parse.OnExamine ->
                    Trigger.OnCommand Command.Examine

                Parse.OnOpen ->
                    Trigger.OnCommand Command.Open

                Parse.OnClose ->
                    Trigger.OnCommand Command.Close

                Parse.OnSpeak ->
                    Trigger.OnCommand Command.Speak

                Parse.OnOperate ->
                    Trigger.OnCommand Command.Operate

                Parse.OnGo ->
                    Trigger.OnCommand Command.Go

                Parse.OnHit ->
                    Trigger.OnCommand Command.Hit

                Parse.OnConsume ->
                    Trigger.OnCommand Command.Consume
    in
    Ok
        { trigger = convertedTrigger
        , statements = statements
        }


convertCondition : String -> ObjectStore -> { trigger : Trigger, statements : List Parse.Statement } -> Result Error { trigger : Trigger, condition : ExpressionBool, statements : List Parse.Statement }
convertCondition localObject objectStore { trigger, statements } =
    case statements of
        (Parse.IfThen expr thenStatements) :: rest ->
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
    | ComparisonTypeMismatch (Result ExpressionError ExprType) Parse.Expr


type TypeError
    = AttributeIsOfTypeInt String String
    | AttributeIsOfTypeString String String
    | AttributeIsOfTypeBool String String
    | LiteralIsOfTypeInt Int
    | LiteralIsOfTypeString String
    | LiteralIsOfTypeBool Bool
    | ComparisonIsOfTypeBool
    | InvalidOperatorForType ExprType Parse.Operator


type RefType
    = RefBool
    | RefInt
    | RefString


type ExprType
    = ExprBool
    | ExprInt
    | ExprString


checkExprType : String -> ObjectStore -> Parse.Expr -> Result ExpressionError ExprType
checkExprType localObject objectStore expr =
    case expr of
        Parse.Comparison leftSide _ rightSide ->
            Ok ExprBool

        Parse.Ref var ->
            let
                ( objKey, attrKey ) =
                    case var of
                        Parse.Local a ->
                            ( localObject, a )

                        Parse.Field o a ->
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

        Parse.LiteralBool _ ->
            Ok ExprBool

        Parse.LiteralString _ ->
            Ok ExprString

        Parse.LiteralInt _ ->
            Ok ExprInt


validateIntExpr : String -> ObjectStore -> Parse.Expr -> Result ExpressionError Expression.ExpressionInt
validateIntExpr localObject objectStore expr =
    case expr of
        Parse.LiteralInt int ->
            Expression.LiteralInt int
                |> Ok

        Parse.LiteralString string ->
            Err (TypeError (LiteralIsOfTypeString string))

        Parse.LiteralBool bool ->
            Err (TypeError (LiteralIsOfTypeBool bool))

        Parse.Comparison _ _ _ ->
            Err (TypeError ComparisonIsOfTypeBool)

        Parse.Ref (Parse.Local attrKey) ->
            case resolveReference objectStore localObject attrKey of
                Ok RefInt ->
                    Ok (Expression.ExpAttributeInt { objId = localObject, key = attrKey })

                Ok RefBool ->
                    Err (TypeError (AttributeIsOfTypeBool localObject attrKey))

                Ok RefString ->
                    Err (TypeError (AttributeIsOfTypeString localObject attrKey))

                Err err ->
                    Err err

        Parse.Ref (Parse.Field objKey attrKey) ->
            case resolveReference objectStore objKey attrKey of
                Ok RefInt ->
                    Ok (Expression.ExpAttributeInt { objId = objKey, key = attrKey })

                Ok RefBool ->
                    Err (TypeError (AttributeIsOfTypeBool localObject attrKey))

                Ok RefString ->
                    Err (TypeError (AttributeIsOfTypeString localObject attrKey))

                Err err ->
                    Err err


validateStringExpr : String -> ObjectStore -> Parse.Expr -> Result ExpressionError Expression.ExpressionString
validateStringExpr localObject objectStore expr =
    case expr of
        Parse.LiteralString int ->
            Expression.LiteralString int
                |> Ok

        Parse.LiteralInt int ->
            Err (TypeError (LiteralIsOfTypeInt int))

        Parse.LiteralBool bool ->
            Err (TypeError (LiteralIsOfTypeBool bool))

        Parse.Comparison _ _ _ ->
            Err (TypeError ComparisonIsOfTypeBool)

        Parse.Ref (Parse.Local attrKey) ->
            case resolveReference objectStore localObject attrKey of
                Ok RefString ->
                    Ok (Expression.ExpAttributeString { objId = localObject, key = attrKey })

                Ok RefBool ->
                    Err (TypeError (AttributeIsOfTypeBool localObject attrKey))

                Ok RefInt ->
                    Err (TypeError (AttributeIsOfTypeInt localObject attrKey))

                Err err ->
                    Err err

        Parse.Ref (Parse.Field objKey attrKey) ->
            case resolveReference objectStore objKey attrKey of
                Ok RefString ->
                    Ok (Expression.ExpAttributeString { objId = objKey, key = attrKey })

                Ok RefBool ->
                    Err (TypeError (AttributeIsOfTypeBool localObject attrKey))

                Ok RefInt ->
                    Err (TypeError (AttributeIsOfTypeInt localObject attrKey))

                Err err ->
                    Err err


validateBoolExpr : String -> ObjectStore -> Parse.Expr -> Result ExpressionError ExpressionBool
validateBoolExpr localObject objectStore expr =
    case expr of
        Parse.LiteralBool bool ->
            Expression.LiteralBool bool
                |> Ok

        Parse.LiteralString string ->
            Err (TypeError (LiteralIsOfTypeString string))

        Parse.LiteralInt int ->
            Err (TypeError (LiteralIsOfTypeInt int))

        Parse.Ref (Parse.Local attrKey) ->
            case resolveReference objectStore localObject attrKey of
                Ok RefBool ->
                    Ok (ExpAttributeBool { objId = localObject, key = attrKey })

                Ok RefInt ->
                    Err (TypeError (AttributeIsOfTypeInt localObject attrKey))

                Ok RefString ->
                    Err (TypeError (AttributeIsOfTypeString localObject attrKey))

                Err err ->
                    Err err

        Parse.Ref (Parse.Field objKey attrKey) ->
            case resolveReference objectStore objKey attrKey of
                Ok RefBool ->
                    Ok (ExpAttributeBool { objId = objKey, key = attrKey })

                Ok RefInt ->
                    Err (TypeError (AttributeIsOfTypeInt localObject attrKey))

                Ok RefString ->
                    Err (TypeError (AttributeIsOfTypeString localObject attrKey))

                Err err ->
                    Err err

        Parse.Comparison leftSide operator rightSide ->
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
                                Parse.EqualTo ->
                                    Ok (Expression.BoolEquals validatedLeft validatedRight)

                                Parse.LessThan ->
                                    Err (TypeError (InvalidOperatorForType ExprBool Parse.LessThan))

                                Parse.GreaterThan ->
                                    Err (TypeError (InvalidOperatorForType ExprBool Parse.GreaterThan))

                                Parse.Contains ->
                                    Err (TypeError (InvalidOperatorForType ExprBool Parse.Contains))

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
                                Parse.EqualTo ->
                                    Ok (Expression.IntEquals validatedLeft validatedRight)

                                Parse.LessThan ->
                                    Ok (Expression.IntLessThan validatedLeft validatedRight)

                                Parse.GreaterThan ->
                                    Ok (Expression.IntGreaterThan validatedLeft validatedRight)

                                Parse.Contains ->
                                    Err (TypeError (InvalidOperatorForType ExprInt Parse.Contains))

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
                                Parse.EqualTo ->
                                    Ok (Expression.StringEquals validatedLeft validatedRight)

                                Parse.LessThan ->
                                    Err (TypeError (InvalidOperatorForType ExprString Parse.LessThan))

                                Parse.GreaterThan ->
                                    Err (TypeError (InvalidOperatorForType ExprString Parse.GreaterThan))

                                Parse.Contains ->
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


convertResults : String -> ObjectStore -> Result Error { trigger : Trigger, condition : ExpressionBool, statements : List Parse.Statement } -> Result Error Script
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


unwrapStatements : Parse.Statement -> List Parse.Statement
unwrapStatements statement =
    case statement of
        Parse.IfThen expr statements ->
            statements

        Parse.Assignment variable expr ->
            [ Parse.Assignment variable expr ]

        Parse.Efct effect ->
            [ Parse.Efct effect ]


convertResultStatement : String -> ObjectStore -> Parse.Statement -> Result Error Script -> Result Error Script
convertResultStatement localObject objectStore statement current =
    case current of
        Ok script ->
            case statement of
                Parse.IfThen expr statements ->
                    Err (StatementError (UnexpectedIfThenStatement (Parse.IfThen expr statements)))

                Parse.Assignment variable expr ->
                    case variable of
                        Parse.Local attrKey ->
                            updateFromReference script expr objectStore localObject attrKey

                        Parse.Field objKey attrKey ->
                            updateFromReference script expr objectStore objKey attrKey

                Parse.Efct effect ->
                    case effect of
                        Parse.PrintText string ->
                            Ok
                                { script
                                    | effects =
                                        script.effects
                                            ++ []
                                }

        Err err ->
            Err err


type StatementError
    = UnexpectedIfThenStatement Parse.Statement


updateFromReference : Script -> Parse.Expr -> ObjectStore -> String -> String -> Result Error Script
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
