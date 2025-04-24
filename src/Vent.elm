module Vent exposing (conditionParser, effectsParser, scriptParser, triggerParser, updatesParser)

import Command
import Effect exposing (Effect)
import Expression exposing (ExpressionBool(..))
import Parser exposing ((|.), (|=), Parser)
import Script exposing (Script)
import Trigger exposing (Trigger)
import Update exposing (Update)


scriptParser : Parser Script
scriptParser =
    Parser.succeed Script
        |= triggerParser
        |= conditionParser
        |= updatesParser
        |= effectsParser


triggerParser : Parser Trigger
triggerParser =
    Parser.succeed identity
        |. Parser.symbol "%"
        |= triggerKeywordParser
        |. Parser.spaces


triggerKeywordParser : Parser Trigger
triggerKeywordParser =
    Parser.succeed identity
        |= (Parser.chompWhile Char.isAlphaNum
                |> Parser.getChompedString
           )
        |> Parser.andThen checkTriggerKeyword


checkTriggerKeyword : String -> Parser Trigger
checkTriggerKeyword stringToCheck =
    case Trigger.fromString stringToCheck of
        Ok trigger ->
            Parser.succeed trigger

        _ ->
            Parser.problem ("Unknown trigger keyword: " ++ stringToCheck)


conditionParser : Parser ExpressionBool
conditionParser =
    Parser.succeed identity
        |. Parser.keyword "if"
        |. Parser.spaces
        |= boolExpressionParser
        |. Parser.spaces
        |. Parser.keyword "then"


boolExpressionParser : Parser ExpressionBool
boolExpressionParser =
    Parser.oneOf
        [ literalBoolParser
        ]


literalBoolParser : Parser ExpressionBool
literalBoolParser =
    Parser.succeed identity
        |= (Parser.chompWhile Char.isAlphaNum
                |> Parser.getChompedString
           )
        |> Parser.andThen checkLiteralBoolKeyword


checkLiteralBoolKeyword : String -> Parser ExpressionBool
checkLiteralBoolKeyword stringToCheck =
    let
        stringToBool : String -> Result String Bool
        stringToBool string =
            case string of
                "true" ->
                    Ok True

                "false" ->
                    Ok False

                other ->
                    Err ("Unknown boolean literal: " ++ other)
    in
    case stringToBool stringToCheck of
        Ok bool ->
            Parser.succeed (LiteralBool bool)

        Err error ->
            Parser.problem error


updatesParser : Parser (List Update)
updatesParser =
    Parser.succeed Update.ClearSelections
        |> Parser.map List.singleton


effectsParser : Parser (List Effect)
effectsParser =
    Parser.succeed (Effect.PrintText "Hi!")
        |> Parser.map List.singleton
