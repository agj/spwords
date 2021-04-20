module Game.Times exposing (Times, start, tick)

import Athlete exposing (Athlete(..))


type Times
    = Times Float Float


start : Times
start =
    Times 1 1


tick : Athlete -> Times -> Times
tick athlete (Times a b) =
    case athlete of
        AthleteA ->
            Times (deplete a) b

        AthleteB ->
            Times a (deplete b)



-- INTERNAL


deplete : Float -> Float
deplete time =
    max 0 (time - 0.01)
