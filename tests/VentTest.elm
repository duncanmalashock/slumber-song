module VentTest exposing (..)

import Command
import Effect exposing (Effect(..))
import Expect
import Expression exposing (ExpressionBool)
import Parser exposing (Problem(..))
import Script exposing (Script)
import Test exposing (Test)
import Trigger exposing (Trigger)
import Update exposing (Update(..))
import Vent


scriptExamples :
    List
        { expect : Result (List Parser.DeadEnd) Script
        , given : String
        }
scriptExamples =
    [ { given = "%any\nif true then\n@skull.isOpen = true\n$printText \"As if by magic, the skull rises.\"\n$printText \"As if by magic, the skull rises.\"\nend"
      , expect =
            Ok
                { trigger = Trigger.OnAny
                , condition = Expression.LiteralBool True
                , updates =
                    [ SetBoolAttribute
                        { attributeKey = "isOpen"
                        , objId = "skull"
                        , value = True
                        }
                    ]
                , effects =
                    [ Effect.PrintText "As if by magic, the skull rises."
                    , Effect.PrintText "As if by magic, the skull rises."
                    ]
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
    , { given = "if @object.attribute then"
      , expect = Ok (Expression.ExpAttributeBool { objId = "object", key = "attribute" })
      }
    ]


updateExamples :
    List
        { expect : Result (List Parser.DeadEnd) Update
        , given : String
        }
updateExamples =
    [ { given = "@skull.isOpen = true"
      , expect = Ok (SetBoolAttribute { objId = "skull", attributeKey = "isOpen", value = True })
      }
    , { given = "@torch.isLit = false"
      , expect = Ok (SetBoolAttribute { objId = "torch", attributeKey = "isLit", value = False })
      }
    ]


effectExamples :
    List
        { expect : Result (List Parser.DeadEnd) Effect
        , given : String
        }
effectExamples =
    [ { given = "$printText \"As if by magic, the skull rises.\""
      , expect = Ok (PrintText "As if by magic, the skull rises.")
      }
    , { given = "$printText \"The torch you lit before has now gone out.\""
      , expect = Ok (PrintText "The torch you lit before has now gone out.")
      }
    ]


effectsExamples :
    List
        { expect : Result (List Parser.DeadEnd) (List Effect)
        , given : String
        }
effectsExamples =
    [ { given = "$printText \"As if by magic, the skull rises.\""
      , expect = Ok [ PrintText "As if by magic, the skull rises." ]
      }
    , { given = "$printText \"The torch you lit before has now gone out.\"\n$printText \"As if by magic, the skull rises.\""
      , expect =
            Ok
                [ PrintText "The torch you lit before has now gone out."
                , PrintText "As if by magic, the skull rises."
                ]
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
        , Test.describe "Vent.updateParser"
            (updateExamples
                |> List.map
                    (\example ->
                        Test.test example.given <|
                            \_ ->
                                Parser.run Vent.updateParser example.given
                                    |> Expect.equal example.expect
                    )
            )
        , Test.describe "Vent.effectParser"
            (effectExamples
                |> List.map
                    (\example ->
                        Test.test example.given <|
                            \_ ->
                                Parser.run Vent.effectParser example.given
                                    |> Expect.equal example.expect
                    )
            )
        , Test.describe "Vent.effectsParser"
            (effectsExamples
                |> List.map
                    (\example ->
                        Test.test example.given <|
                            \_ ->
                                Parser.run Vent.effectsParser example.given
                                    |> Expect.equal example.expect
                    )
            )
        ]
