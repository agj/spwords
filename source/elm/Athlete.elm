module Athlete exposing (Athlete(..), opposite)


type Athlete
    = AthleteA
    | AthleteB


opposite : Athlete -> Athlete
opposite athlete =
    case athlete of
        AthleteA ->
            AthleteB

        AthleteB ->
            AthleteA
