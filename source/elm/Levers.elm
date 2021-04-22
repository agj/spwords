module Levers exposing
    ( Factor
    , Milliseconds
    , Ticks
    , computerLetterDelay
    , computerLetterErrorProbability
    , computerWordDelay
    , computerWordErrorFactor
    , tickInterval
    , timeDepletionRate
    )

import Speed exposing (Speed)


type alias Milliseconds =
    Float


type alias Ticks =
    Int


{-| Indicates a Float that will get multiplied against another number in order to adjust it.
-}
type alias Factor =
    Float


type alias FractionPerTick =
    Float


{-| The duration of one tick of the ticker.
-}
tickInterval : Speed -> Milliseconds
tickInterval speed =
    case speed of
        Speed.Normal ->
            80

        Speed.Slow ->
            100


timeDepletionRate : FractionPerTick
timeDepletionRate =
    perSToPerTick 0.05


{-| How long the computer “thinks” before starting to type their word.
-}
computerWordDelay : { min : Ticks, max : Ticks }
computerWordDelay =
    { min = msToTicks 200
    , max = msToTicks 2500
    }


{-| Gets multiplied with a probability based on how many words the dictionary has that fit the current constraints.
The bigger the number, the higher the probability of picking the wrong word.
-}
computerWordErrorFactor : Factor
computerWordErrorFactor =
    50


{-| How long the computer takes to type each single letter in the word.
-}
computerLetterDelay : { min : Ticks, max : Ticks }
computerLetterDelay =
    { min = msToTicks 20
    , max = msToTicks 120
    }


{-| How likely it is for the computer to make a mistake typing a letter.
1 is 100%.
-}
computerLetterErrorProbability : Float
computerLetterErrorProbability =
    0.005



-- INTERNAL


msToTicks : Milliseconds -> Ticks
msToTicks ms =
    ms / tickInterval Speed.Normal |> round |> max 1


perSToPerTick : Float -> FractionPerTick
perSToPerTick fr =
    fr / 1000 * tickInterval Speed.Normal
