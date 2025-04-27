module Vent.Parse exposing
    ( Effect(..)
    , Error(..)
    , Expr(..)
    , Operator(..)
    , ParseResult
    , Script(..)
    , Statement(..)
    , Trigger(..)
    , Variable(..)
    , execute
    , scriptParser
    , statementParser
    , statementsParser
    , triggerParser
    )

import Parser.Advanced as Parser exposing ((|.), (|=))


type alias Parser a =
    Parser.Parser () Error a


type Error
    = UnknownError
    | ExpectedKeyword String
    | ExpectedSymbol String
    | AssignmentHasNoRightSide
    | ExpectedLowerCaseVariable
    | IncompleteComparison
    | ReservedKeyword
    | IfThenIsMissingCondition
    | ReachedEndOfFile
    | UnknownEffect


execute : String -> Result Error Script
execute input =
    case Parser.run scriptParser input of
        Ok script ->
            Ok script

        Err deadEnds ->
            let
                maybeHead =
                    deadEnds
                        |> List.reverse
                        |> List.head
            in
            case maybeHead of
                Just { problem } ->
                    Err problem

                Nothing ->
                    Err UnknownError



-- Script


type Script
    = Script Trigger (List Statement)


scriptParser : Parser Script
scriptParser =
    Parser.succeed Script
        |= triggerParser
        |= statementsParser Nothing



-- Trigger


type Trigger
    = OnAny
    | OnExamine
    | OnOpen
    | OnClose
    | OnSpeak
    | OnOperate
    | OnGo
    | OnHit
    | OnConsume


triggerParser : Parser Trigger
triggerParser =
    Parser.succeed identity
        |. symbol "%"
        |= triggerKeywordParser
        |. spacesOrNewline


triggerKeywordParser : Parser Trigger
triggerKeywordParser =
    Parser.oneOf
        [ Parser.succeed OnAny
            |. keyword "any"
        , Parser.succeed OnExamine
            |. keyword "examine"
        , Parser.succeed OnOpen
            |. keyword "open"
        , Parser.succeed OnClose
            |. keyword "close"
        , Parser.succeed OnSpeak
            |. keyword "speak"
        , Parser.succeed OnOperate
            |. keyword "operate"
        , Parser.succeed OnGo
            |. keyword "go"
        , Parser.succeed OnHit
            |. keyword "hit"
        , Parser.succeed OnConsume
            |. keyword "consume"
        ]



-- Statements


type Statement
    = Assignment Variable Expr
    | IfThen Expr (List Statement)
    | Efct Effect


type Expr
    = -- Operations
      Comparison Expr Operator Expr
      -- Sub expressions
    | Ref Variable
    | LiteralBool Bool


type Variable
    = Local String
    | Field String String


type Operator
    = EqualTo
    | LessThan
    | GreaterThan


type Effect
    = PrintText String


statementsParser : Maybe String -> Parser (List Statement)
statementsParser maybeTermKeyword =
    let
        loop : List Statement -> Parser Step
        loop statements =
            Parser.oneOf
                [ Parser.succeed
                    (Parser.Done (List.reverse statements))
                    |. (case maybeTermKeyword of
                            Just theKeyword ->
                                keyword theKeyword

                            Nothing ->
                                Parser.end ReachedEndOfFile
                       )
                , Parser.succeed
                    (\statement -> Parser.Loop (statement :: statements))
                    |= statementParser
                    |. spacesOrNewline
                ]
    in
    Parser.loop [] loop


type alias Step =
    Parser.Step (List Statement) (List Statement)


statementParser : Parser Statement
statementParser =
    Parser.oneOf
        [ ifThenParser
        , assignmentParser
        , effectParser
        ]


ifThenParser : Parser Statement
ifThenParser =
    Parser.succeed IfThen
        |. keyword "if"
        |. spaces
        |= attempt exprParser IfThenIsMissingCondition
        |. spaces
        |. keyword "then"
        |. spacesOrNewline
        |= statementsParser (Just "end")


assignmentParser : Parser Statement
assignmentParser =
    Parser.succeed Assignment
        |= variableParser
        |. spaces
        |. symbol "="
        |. spaces
        |= attempt exprParser AssignmentHasNoRightSide


effectParser : Parser Statement
effectParser =
    Parser.succeed Efct
        |. symbol "$"
        |= attempt
            (Parser.oneOf
                [ printTextParser
                ]
            )
            UnknownEffect


printTextParser : Parser Effect
printTextParser =
    Parser.succeed PrintText
        |. symbol "printText"
        |. spaces
        |= stringLiteral


variableParser : Parser Variable
variableParser =
    Parser.oneOf
        [ Parser.succeed Field
            |. symbol "@"
            |= varNameParser
            |. symbol "."
            |= varNameParser
        , Parser.succeed Local
            |= varNameParser
        ]


varNameParser : Parser String
varNameParser =
    Parser.succeed ()
        |. Parser.chompIf Char.isLower ExpectedLowerCaseVariable
        |. Parser.chompWhile Char.isAlpha
        |> Parser.getChompedString


exprParser : Parser Expr
exprParser =
    Parser.succeed toExpr
        |= subExprParser ReservedKeyword
        |. spaces
        |= maybeComparisonParser


toExpr : Expr -> Maybe ( Operator, Expr ) -> Expr
toExpr left maybe =
    case maybe of
        Nothing ->
            left

        Just ( op, right ) ->
            Comparison left op right


maybeComparisonParser : Parser (Maybe ( Operator, Expr ))
maybeComparisonParser =
    Parser.oneOf
        [ Parser.succeed (\op right -> Just ( op, right ))
            |= operatorParser
            |. spaces
            |= subExprParser IncompleteComparison
        , Parser.succeed Nothing
        ]


operatorParser : Parser Operator
operatorParser =
    Parser.oneOf
        [ Parser.succeed EqualTo
            |. symbol "=="
        , Parser.succeed LessThan
            |. symbol "<"
        , Parser.succeed GreaterThan
            |. symbol ">"
        ]


subExprParser : Error -> Parser Expr
subExprParser error =
    Parser.oneOf
        [ failForReservedKeywords error
        , Parser.succeed (LiteralBool True)
            |. keyword "true"
        , Parser.succeed (LiteralBool False)
            |. keyword "false"
        , Parser.succeed Ref
            |= variableParser
        ]


failForReservedKeywords : Error -> Parser a
failForReservedKeywords error =
    Parser.oneOf (List.map keyword reservedKeywords)
        |> Parser.andThen
            (\location ->
                Parser.problem error
            )


reservedKeywords : List String
reservedKeywords =
    [ "if"
    , "end"
    , "then"
    ]



-- Helpers


type alias ParseResult a =
    Result (List DeadEnd) a


type alias DeadEnd =
    Parser.DeadEnd () Error


attempt : Parser value -> Error -> Parser value
attempt innerParser err =
    Parser.oneOf
        [ innerParser
        , Parser.problem err
        ]


keyword : String -> Parser ()
keyword str =
    Parser.keyword (Parser.Token str (ExpectedKeyword str))


symbol : String -> Parser ()
symbol str =
    Parser.symbol (Parser.Token str (ExpectedSymbol str))


spaces : Parser ()
spaces =
    Parser.chompWhile isSpaceChar


spacesOrNewline : Parser ()
spacesOrNewline =
    Parser.chompWhile (\char -> isSpaceChar char || isNewlineChar char)


isSpaceChar : Char -> Bool
isSpaceChar char =
    char == ' '


isNewlineChar : Char -> Bool
isNewlineChar char =
    char == '\n'


stringLiteral : Parser String
stringLiteral =
    Parser.succeed identity
        |. symbol "\""
        |= (Parser.chompWhile (\char -> char /= '"')
                |> Parser.getChompedString
           )
        |. symbol "\""
