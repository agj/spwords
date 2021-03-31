module Score exposing (..)

import Athlete exposing (Athlete)


type Score
    = PlayingScore PlayingScore
    | WinnerScore Athlete Points


type alias PlayingScore =
    ( Points, Points )


type Points
    = Love
    | One
    | Two


emptyPlayingScore : PlayingScore
emptyPlayingScore =
    ( Love, Love )
