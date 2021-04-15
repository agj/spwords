module ComputerThought exposing
    ( ComputerThought
    , create
    , getInput
    , isFinished
    , tick
    )

import Constraints exposing (Constraints)
import Words exposing (Words)


type ComputerThought
    = ComputerThought
        { word : String
        , input : String
        , ticks : Int
        }


create : Words -> Constraints -> ComputerThought
create words cnts =
    ComputerThought
        { word = "test"
        , input = ""
        , ticks = 0
        }


tick : ComputerThought -> ComputerThought
tick ((ComputerThought ct) as thought) =
    let
        newTicks =
            ct.ticks + 1
    in
    if newTicks > 10 then
        typeLetter thought

    else
        ComputerThought { ct | ticks = newTicks }


isFinished : ComputerThought -> Bool
isFinished (ComputerThought ct) =
    String.length ct.input >= String.length ct.word


getInput : ComputerThought -> String
getInput (ComputerThought ct) =
    ct.input



-- INTERNAL


typeLetter : ComputerThought -> ComputerThought
typeLetter (ComputerThought ct) =
    ComputerThought
        { ct
            | input = String.left (String.length ct.input + 1) ct.input
            , ticks = 0
        }
