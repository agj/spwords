module Levers exposing
    ( Milliseconds
    , Ticks
    , computerLetterDelay
    , computerLetterErrorProbability
    , computerWordDelay
    , tickInterval
    )


type alias Milliseconds =
    Float


type alias Ticks =
    Int


tickInterval : Milliseconds
tickInterval =
    80


computerWordDelay : { min : Ticks, max : Ticks }
computerWordDelay =
    { min = msToTicks 200
    , max = msToTicks 2500
    }


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
