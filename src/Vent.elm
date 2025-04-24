module Vent exposing (conditionParser, effectParser, scriptParser, triggerParser, updateParser)

import Command
import Effect exposing (Effect)
import Expression exposing (ExpressionBool(..))
import Parser exposing ((|.), (|=), Parser)
import Script exposing (Script)
import Set
import Trigger exposing (Trigger)
import Update exposing (Update)



-- VENT: the Vintage Exploratory-Narrative Toolkit


scriptParser : Parser Script
scriptParser =
    Parser.succeed Script
        |= triggerParser
        |= conditionParser
        |= updatesParser
        |= effectsParser
        |. Parser.keyword "end"
        |. Parser.spaces
        |. Parser.end


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
        |. Parser.spaces


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
    updateParser
        |> Parser.map List.singleton


updateParser : Parser Update
updateParser =
    Parser.oneOf
        [ setAttributeUpdateParser
        ]
        |. Parser.spaces


setAttributeUpdateParser : Parser Update
setAttributeUpdateParser =
    let
        objVarParser : Parser String
        objVarParser =
            Parser.variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                , reserved = Set.fromList []
                }

        attrVarParser : Parser String
        attrVarParser =
            Parser.variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                , reserved = Set.fromList []
                }

        boolLiteralParser : Parser Bool
        boolLiteralParser =
            Parser.oneOf
                [ Parser.succeed True
                    |. Parser.keyword "true"
                , Parser.succeed False
                    |. Parser.keyword "false"
                ]

        buildSetBoolAttribute : String -> String -> Bool -> Update
        buildSetBoolAttribute objId attrId value =
            Update.SetBoolAttribute
                { objId = objId
                , attributeKey = attrId
                , value = value
                }
    in
    Parser.succeed buildSetBoolAttribute
        |. Parser.symbol "@"
        |= objVarParser
        |. Parser.symbol "."
        |= attrVarParser
        |. Parser.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |= boolLiteralParser


effectsParser : Parser (List Effect)
effectsParser =
    effectParser
        |> Parser.map List.singleton


effectParser : Parser Effect
effectParser =
    Parser.oneOf
        [ printTextEffectParser
        ]
        |. Parser.spaces


printTextEffectParser : Parser Effect
printTextEffectParser =
    Parser.succeed Effect.PrintText
        |. Parser.keyword "$printText"
        |. Parser.spaces
        |. Parser.symbol "\""
        |= (Parser.chompUntil "\""
                |> Parser.getChompedString
           )
        |. Parser.symbol "\""
