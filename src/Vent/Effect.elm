module Vent.Effect exposing (Effect(..), toInstructions)

import MacOS.Instruction as Instruction exposing (Instruction(..))


type Effect
    = PrintText { narrationObjectId : String } String


toInstructions : Effect -> List (Instruction msg)
toInstructions effect =
    case effect of
        PrintText { narrationObjectId } textToPrint ->
            [ Instruction.UpdateObjectText
                { objectId = narrationObjectId
                , text = textToPrint
                }
            ]
