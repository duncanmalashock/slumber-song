module VentTest exposing (..)

import Command
import Effect exposing (Effect)
import Expect
import Expression exposing (ExpressionBool)
import Parser exposing (Problem(..))
import Script exposing (Script)
import Test exposing (Test)
import Trigger exposing (Trigger)
import Update exposing (Update)
import Vent


scriptExamples :
    List
        { expect : Result (List Parser.DeadEnd) Script
        , given : String
        }
scriptExamples =
    [ { given = "%any\nif true then\n^clearSelections\n$printText \"Hi!\"\nend"
      , expect =
            Ok
                { trigger = Trigger.OnAny
                , condition = Expression.LiteralBool True
                , updates = [ Update.ClearSelections ]
                , effects = [ Effect.PrintText "Hi!" ]
                }
      }
    ]


triggerExamples :
    List
        { expect : Result (List Parser.DeadEnd) Trigger
        , given : String
        }
triggerExamples =
    [ { given = "%any"
      , expect = Ok Trigger.OnAny
      }
    , { given = "%open"
      , expect = Ok (Trigger.OnCommand Command.Open)
      }
    , { given = "%operate"
      , expect = Ok (Trigger.OnCommand Command.Operate)
      }
    , { given = "%banana"
      , expect =
            Err
                [ { col = 8
                  , problem = Problem "Unknown trigger keyword: banana"
                  , row = 1
                  }
                ]
      }
    ]


conditionExamples :
    List
        { expect : Result (List Parser.DeadEnd) ExpressionBool
        , given : String
        }
conditionExamples =
    [ { given = "if true then"
      , expect = Ok (Expression.LiteralBool True)
      }
    , { given = "if false then"
      , expect = Ok (Expression.LiteralBool False)
      }
    ]


suite : Test
suite =
    Test.describe "Vent"
        [ Test.describe "Vent.scriptParser"
            (scriptExamples
                |> List.map
                    (\example ->
                        Test.test example.given <|
                            \_ ->
                                Parser.run Vent.scriptParser example.given
                                    |> Expect.equal example.expect
                    )
            )
        , Test.describe "Vent.triggerParser"
            (triggerExamples
                |> List.map
                    (\example ->
                        Test.test example.given <|
                            \_ ->
                                Parser.run Vent.triggerParser example.given
                                    |> Expect.equal example.expect
                    )
            )
        , Test.describe "Vent.conditionParser"
            (conditionExamples
                |> List.map
                    (\example ->
                        Test.test example.given <|
                            \_ ->
                                Parser.run Vent.conditionParser example.given
                                    |> Expect.equal example.expect
                    )
            )
        ]
