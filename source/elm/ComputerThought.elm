module ComputerThought exposing
    ( ComputerThought
    , create
    , getInput
    , isFinished
    , tick
    )

import Constraints exposing (Constraints)
import Levers
import Random
import Utils exposing (stringCharAt)
import Words exposing (Words)


type ComputerThought
    = ComputerThought
        { word : String
        , input : String
        , ticks : Int
        , nextLetter : NextLetter
        , seed : Random.Seed
        }


type alias NextLetter =
    { ticks : Int
    , letter : Char
    }


create : Words -> Constraints -> Random.Seed -> ( ComputerThought, Random.Seed )
create words cnts seed =
    let
        word =
            getWord words cnts

        ( seed1, outSeed ) =
            Random.step Random.independentSeed seed

        ( nextLetter, newSeed ) =
            getNextLetter word 0 seed1
    in
    ( ComputerThought
        { word = "test"
        , input = ""
        , ticks = 0
        , nextLetter = nextLetter
        , seed = newSeed
        }
    , outSeed
    )


tick : ComputerThought -> ComputerThought
tick ((ComputerThought ct) as thought) =
    let
        newTicks =
            ct.ticks + 1
    in
    if newTicks >= ct.nextLetter.ticks then
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


getWord : Words -> Constraints -> String
getWord words cnts =
    "test"


getNextLetter : String -> Int -> Random.Seed -> ( NextLetter, Random.Seed )
getNextLetter word index seed =
    let
        letter =
            stringCharAt index word
                |> Maybe.withDefault '?'

        ( ticks, newSeed ) =
            if index == 0 then
                Random.step startLetterDelayGenerator seed

            else
                Random.step letterDelayGenerator seed
    in
    ( { ticks = ticks, letter = letter }
    , newSeed
    )


typeLetter : ComputerThought -> ComputerThought
typeLetter (ComputerThought ct) =
    let
        newInput =
            ct.input ++ (ct.nextLetter.letter |> String.fromChar)

        ( nextLetter, newSeed ) =
            getNextLetter ct.word (String.length newInput) ct.seed
    in
    ComputerThought
        { ct
            | input = newInput
            , ticks = 0
            , nextLetter = nextLetter
            , seed = newSeed
        }


letterDelayGenerator =
    Random.int Levers.computerLetterDelay.min Levers.computerLetterDelay.max


startLetterDelayGenerator =
    Random.int Levers.computerWordDelay.min Levers.computerWordDelay.max
