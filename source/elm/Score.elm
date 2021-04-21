module Score exposing (..)

import Athlete exposing (Athlete(..))


type Score
    = PlayingScore PlayingScore
    | WinnerScore Athlete Points


type alias PlayingScore =
    ( Points, Points )


type Points
    = Love
    | One
    | Two


winPoints : Int
winPoints =
    3


winPointsString : String
winPointsString =
    "three"


emptyPlayingScore : PlayingScore
emptyPlayingScore =
    ( Love, Love )


increaseScore : Athlete -> PlayingScore -> Score
increaseScore athlete score =
    case ( athlete, score ) of
        ( AthleteA, ( Love, sb ) ) ->
            PlayingScore ( One, sb )

        ( AthleteA, ( One, sb ) ) ->
            PlayingScore ( Two, sb )

        ( AthleteA, ( Two, sb ) ) ->
            WinnerScore AthleteA sb

        ( AthleteB, ( sa, Love ) ) ->
            PlayingScore ( sa, One )

        ( AthleteB, ( sa, One ) ) ->
            PlayingScore ( sa, Two )

        ( AthleteB, ( sa, Two ) ) ->
            WinnerScore AthleteB sa


intFromPoints : Points -> Int
intFromPoints points =
    case points of
        Love ->
            0

        One ->
            1

        Two ->
            2


stringFromPoints : Points -> String
stringFromPoints points =
    case points of
        Love ->
            "love"

        One ->
            "one"

        Two ->
            "two"
