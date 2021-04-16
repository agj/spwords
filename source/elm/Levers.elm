module Levers exposing
    ( Milliseconds
    , Ticks
    , computerLetterDelay
    , computerLetterErrorProbability
    , computerWordDelay
    , computerWordErrorFactor
    , tickInterval
    )


type alias Milliseconds =
    Float


type alias Ticks =
    Int


type alias Factor =
    Float


tickInterval : Milliseconds
tickInterval =
    80


computerWordDelay : { min : Ticks, max : Ticks }
computerWordDelay =
    { min = msToTicks 200
    , max = msToTicks 2500
    }


computerWordErrorFactor : Factor
computerWordErrorFactor =
    100


computerLetterDelay : { min : Ticks, max : Ticks }
computerLetterDelay =
    { min = msToTicks 20
    , max = msToTicks 150
    }


computerLetterErrorProbability =
    0.005



-- INTERNAL


msToTicks : Milliseconds -> Ticks
msToTicks ms =
    ms / tickInterval |> round |> max 1
