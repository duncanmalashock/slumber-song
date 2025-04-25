module VentTest exposing (..)

import Expect
import Parser.Advanced as Parser
import Test exposing (Test)
import Vent.Parse as Vent


triggerExamples :
    List
        { expect : Vent.ParseResult Vent.Trigger
        , given : String
        }
triggerExamples =
    [ { given = "%any"
      , expect = Ok Vent.OnAny
      }
    , { given = "%operate"
      , expect = Ok Vent.OnOperate
      }
    , { given = "open"
      , expect = Err [ { col = 1, contextStack = [], problem = Vent.ExpectedSymbol "%", row = 1 } ]
      }
    ]


statementExamples :
    List
        { expect : Vent.ParseResult Vent.Statement
        , given : String
        }
statementExamples =
    [ { given = "if isOpen == true then end"
      , expect = Ok (Vent.IfThen (Vent.Comparison (Vent.Ref (Vent.Local "isOpen")) Vent.EqualTo (Vent.LiteralBool True)) [])
      }
    , { given = "isOpen = true"
      , expect = Ok (Vent.Assignment (Vent.Local "isOpen") (Vent.LiteralBool True))
      }
    , { given = "@skull.isOpen = true"
      , expect = Ok (Vent.Assignment (Vent.Field "skull" "isOpen") (Vent.LiteralBool True))
      }
    , { given = "$printText \"As if by magic, the skull rises.\""
      , expect = Ok (Vent.Efct (Vent.PrintText "As if by magic, the skull rises."))
      }
    ]


suite : Test
suite =
    Test.describe "Vent"
        [ Test.describe "Vent.triggerParser"
            (triggerExamples
                |> List.map
                    (\example ->
                        Test.test example.given <|
                            \_ ->
                                Parser.run Vent.triggerParser example.given
                                    |> Expect.equal example.expect
                    )
            )
        , Test.describe "Vent.statementParser"
            (statementExamples
                |> List.map
                    (\example ->
                        Test.test example.given <|
                            \_ ->
                                Parser.run Vent.statementParser example.given
                                    |> Expect.equal example.expect
                    )
            )
        ]
