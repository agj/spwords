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
import Utils exposing (randomItem, stringCharAt)
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
        ( seed1, outSeed ) =
            Random.step Random.independentSeed seed

        ( word, seed2 ) =
            getWord seed1 words cnts

        ( nextLetter, newSeed ) =
            getNextLetter word 0 seed2
    in
    ( ComputerThought
        { word = word
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


getNextLetter : String -> Int -> Random.Seed -> ( NextLetter, Random.Seed )
getNextLetter word index seed =
    let
        letter =
            stringCharAt index word
                |> Maybe.withDefault '?'

        ( ticks, newSeed ) =
            if index == 0 then
                Random.step initialDelayGenerator seed

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


getWord : Random.Seed -> Words -> Constraints -> ( String, Random.Seed )
getWord seed words cnts =
    let
        initial =
            Constraints.getInitial cnts

        incorporatesM =
            Constraints.getIncorporates cnts

        wordsWithInitial =
            Words.getByInitial initial words

        filteredWords =
            case incorporatesM of
                Nothing ->
                    wordsWithInitial

                Just incorporates ->
                    wordsWithInitial
                        |> List.filter (String.contains (String.fromChar incorporates))

        errorProbability =
            1 / (List.length filteredWords |> toFloat)

        ( incorporatesError, seed1 ) =
            Random.step
                (Random.float 0 1
                    |> Random.map (\n -> n < errorProbability)
                )
                seed

        selectedWords =
            if incorporatesError then
                wordsWithInitial

            else
                filteredWords
    in
    randomItem seed1 selectedWords
        |> Tuple.mapFirst (Maybe.withDefault "?")


letterDelayGenerator =
    Random.int Levers.computerLetterDelay.min Levers.computerLetterDelay.max


initialDelayGenerator =
    Random.int Levers.computerWordDelay.min Levers.computerWordDelay.max
