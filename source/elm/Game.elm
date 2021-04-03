module Game exposing
    ( Game(..)
    , Turn(..)
    , assessment
    , athleteInput
    , athleteInputDone
    , endRound
    , getActiveAthlete
    , getAnnouncement
    , newRound
    , showRules
    , startGame
    , startPlay
    , startRound
    , tally
    )

import Announcement exposing (Announcement)
import Athlete exposing (..)
import Constraints exposing (Constraints)
import Doc.Paragraph exposing (Paragraph)
import Random
import Score exposing (PlayingScore, Points, Score)
import Texts
import Utils
import Words exposing (Words)


type Game
    = Hotseat Turn
    | Single Turn


type Turn
    = GameStart Announcement
    | Rules Announcement
    | RoundStart PlayingScore Athlete Constraints Announcement
    | Play PlayingScore Athlete String Constraints
    | PlayCorrect PlayingScore Athlete Constraints Announcement
    | PlayWrong Score Athlete Constraints Announcement
    | RoundEnd Score Athlete Announcement
    | Tally PlayingScore Athlete Announcement
    | Assessment PlayingScore Athlete Announcement
    | NewRound PlayingScore Athlete Announcement
    | End Athlete Points Announcement


getAnnouncement : Game -> Maybe Announcement
getAnnouncement game =
    case game of
        Hotseat turn ->
            case turn of
                GameStart ann ->
                    Just ann

                Rules ann ->
                    Just ann

                RoundStart _ _ _ ann ->
                    Just ann

                PlayCorrect _ _ _ ann ->
                    Just ann

                PlayWrong _ _ _ ann ->
                    Just ann

                RoundEnd _ _ ann ->
                    Just ann

                Tally _ _ ann ->
                    Just ann

                Assessment _ _ ann ->
                    Just ann

                NewRound _ _ ann ->
                    Just ann

                End _ _ ann ->
                    Just ann

                Play _ _ _ _ ->
                    Nothing

        Single turn ->
            Debug.todo "Single mode not implemented."


getActiveAthlete : Game -> Maybe Athlete
getActiveAthlete game =
    case game of
        Hotseat (Play _ athlete _ _) ->
            Just athlete

        Hotseat _ ->
            Nothing

        Single _ ->
            Debug.todo "Single mode not implemented"



-- GAME TURN GENERATION


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


athleteInput : { input : String, words : Words, score : PlayingScore, athlete : Athlete, constraints : Constraints, seed : Random.Seed } -> ( Game, Random.Seed )
athleteInput { input, words, score, athlete, constraints, seed } =
    let
        playWrongWith messageFn =
            playWrong
                { messageFn = messageFn
                , score = score
                , athlete = athlete
                , constraints = constraints
                , seed = seed
                }
    in
    case Constraints.checkCandidate input constraints words of
        Constraints.CandidateCorrect ->
            ( Hotseat (Play score athlete input constraints)
            , seed
            )

        Constraints.CandidateInitialWrong ->
            playWrongWith Texts.initialWrong

        Constraints.CandidateNotAWord ->
            playWrongWith Texts.notAWord


athleteInputDone : { input : String, words : Words, constraints : Constraints, score : PlayingScore, athlete : Athlete, seed : Random.Seed } -> Maybe ( Game, Random.Seed )
athleteInputDone { input, words, constraints, score, athlete, seed } =
    let
        playWrongWith messageFn =
            Just
                (playWrong
                    { messageFn = messageFn
                    , score = score
                    , athlete = athlete
                    , constraints = constraints
                    , seed = seed
                    }
                )
    in
    if String.length input > 0 then
        case Constraints.check input constraints words of
            Constraints.InputCorrect ->
                let
                    newCnts =
                        constraints |> Constraints.pushPlayed input
                in
                Just
                    (playCorrect
                        { constraints = newCnts
                        , score = score
                        , athlete = athlete
                        , seed = seed
                        }
                    )

            Constraints.InputInitialWrong ->
                playWrongWith Texts.initialWrong

            Constraints.InputIncorporatesWrong ->
                playWrongWith Texts.incorporatesWrong

            Constraints.InputAlreadyPlayed ->
                playWrongWith Texts.alreadyPlayed

            Constraints.InputNotAWord ->
                playWrongWith Texts.notAWord

    else
        Nothing


endRound : { winner : Athlete, score : Score, seed : Random.Seed } -> ( Game, Random.Seed )
endRound { winner, score, seed } =
    let
        ( message, newSeed ) =
            Texts.roundEnd
                { winner = winner
                , athleteA = "left"
                , athleteB = "right"
                , seed = seed
                }

        newGame =
            Hotseat (RoundEnd score winner (message |> Announcement.create))
    in
    ( newGame, newSeed )


tally : { score : PlayingScore, athlete : Athlete, seed : Random.Seed } -> ( Game, Random.Seed )
tally { score, athlete, seed } =
    let
        ( message, newSeed ) =
            Texts.tally
                { athleteA = "left"
                , athleteB = "right"
                , pointsA = Tuple.first score
                , pointsB = Tuple.second score
                , seed = seed
                }

        newGame =
            Hotseat (Tally score athlete (message |> Announcement.create))
    in
    ( newGame, newSeed )


assessment : { score : PlayingScore, athlete : Athlete, seed : Random.Seed } -> ( Game, Random.Seed )
assessment { score, athlete, seed } =
    let
        ( message, newSeed ) =
            case score of
                ( pointsA, pointsB ) ->
                    if pointsA == pointsB then
                        Texts.tie
                            { points = pointsA
                            , seed = seed
                            }

                    else
                        Texts.winning
                            { winner =
                                if (pointsA |> Score.intFromPoints) > (pointsB |> Score.intFromPoints) then
                                    AthleteA

                                else
                                    AthleteB
                            , athleteA = "left"
                            , athleteB = "right"
                            , seed = seed
                            }

        newGame =
            Hotseat (Assessment score athlete (message |> Announcement.create))
    in
    ( newGame, newSeed )


newRound : { athlete : Athlete, score : PlayingScore, seed : Random.Seed } -> ( Game, Random.Seed )
newRound { athlete, score, seed } =
    let
        ( message, newSeed ) =
            Texts.newRound seed

        newGame =
            Hotseat (NewRound score athlete (message |> Announcement.create))
    in
    ( newGame, newSeed )



-- INTERNAL


playCorrect : { constraints : Constraints, score : PlayingScore, athlete : Athlete, seed : Random.Seed } -> ( Game, Random.Seed )
playCorrect { constraints, score, athlete, seed } =
    let
        newCnts =
            Constraints.rally
                { initial = Constraints.getInitial constraints
                , incorporates =
                    Constraints.getIncorporates constraints
                        |> Maybe.withDefault '?'
                , played = Constraints.getPlayed constraints
                }

        ( message, newSeed ) =
            Texts.interjection seed

        newGame =
            Hotseat (PlayCorrect score athlete newCnts (message |> Announcement.create))
    in
    ( newGame, newSeed )


playWrong : { messageFn : Texts.MistakeArguments -> ( Paragraph, Random.Seed ), score : PlayingScore, athlete : Athlete, constraints : Constraints, seed : Random.Seed } -> ( Game, Random.Seed )
playWrong { messageFn, score, athlete, constraints, seed } =
    let
        newScore =
            Score.increaseScore (Utils.oppositeAthlete athlete) score

        ( message, newSeed ) =
            messageFn
                { initial = constraints |> Constraints.getInitial
                , incorporates = constraints |> Constraints.getIncorporates
                , seed = seed
                }

        newGame =
            Hotseat
                (PlayWrong
                    newScore
                    athlete
                    constraints
                    (message |> Announcement.create)
                )
    in
    ( newGame, newSeed )


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
