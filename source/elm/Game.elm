module Game exposing (..)

import Announcement exposing (Announcement)
import Athlete exposing (..)
import Constraints exposing (Constraints)
import Random
import Score exposing (PlayingScore, Points, Score)
import Texts
import Utils


type Game
    = Hotseat Turn
    | Single Turn


type Turn
    = GameStart Announcement
    | Rules Announcement
    | RoundStart PlayingScore Athlete Constraints Announcement
    | Play PlayingScore Athlete String Constraints
    | PlayCorrect Score Athlete Constraints Announcement
    | PlayWrong Score Athlete Constraints Announcement
    | RoundEnd Score Athlete Announcement
    | NewRound PlayingScore Athlete Announcement
    | Tally Score Athlete Announcement
    | End Athlete Points Announcement


startGame : Game
startGame =
    Hotseat
        (GameStart
            (Texts.gameStart
                { athleteA = "left"
                , athleteB = "right"
                }
                |> Announcement.create
            )
        )


showRules : Game
showRules =
    Hotseat (Rules (Texts.rules |> Announcement.create))


startRound : { athlete : Athlete, score : PlayingScore, seed : Random.Seed } -> ( Game, Random.Seed )
startRound { athlete, score, seed } =
    let
        ( initial, seed1 ) =
            randomLetter seed Texts.alphabet

        ( message, newSeed ) =
            Texts.roundStart
                { turnAthlete = athlete
                , seed = seed1
                , turn =
                    case athlete of
                        AthleteA ->
                            "left"

                        AthleteB ->
                            "right"
                , initial = initial
                }
    in
    ( Hotseat
        (RoundStart
            score
            athlete
            (Constraints.serve initial)
            (Announcement.create message)
        )
    , newSeed
    )


startPlay : { score : PlayingScore, athlete : Athlete, constraints : Constraints } -> Game
startPlay { score, athlete, constraints } =
    Hotseat (Play score athlete "" constraints)


athleteInput : { input : String, previousInput : String, score : PlayingScore, athlete : Athlete, constraints : Constraints } -> Game
athleteInput { input, previousInput, score, athlete, constraints } =
    Hotseat (Play score athlete (previousInput ++ input) constraints)


endRound : { winner : Athlete, athleteA : String, athleteB : String, score : Score, seed : Random.Seed } -> ( Game, Random.Seed )
endRound { winner, athleteA, athleteB, score, seed } =
    let
        ( message, newSeed ) =
            Texts.roundEnd
                { winner = winner
                , athleteA = athleteA
                , athleteB = athleteB
                , seed = seed
                }

        newGame =
            Hotseat (RoundEnd score winner (message |> Announcement.create))
    in
    ( newGame, newSeed )



-- INTERNAL


randomLetter : Random.Seed -> String -> ( Char, Random.Seed )
randomLetter seed alphabet =
    Random.step (letterGenerator alphabet) seed


letterGenerator : String -> Random.Generator Char
letterGenerator alphabet =
    Random.int 0 (String.length alphabet - 1)
        |> Random.map (indexToLetter alphabet)


indexToLetter : String -> Int -> Char
indexToLetter alpha n =
    Utils.stringCharAt n alpha
        |> Maybe.withDefault '?'
