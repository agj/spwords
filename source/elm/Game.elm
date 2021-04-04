module Game exposing
    ( Game(..)
    , Turn(..)
    , athleteInput
    , getActiveAthlete
    , getAnnouncement
    , skip
    , startGame
    , tick
    )

import Athlete exposing (..)
import Constraints exposing (Constraints)
import Doc.Paragraph exposing (Paragraph)
import Random
import Score exposing (PlayingScore, Points, Score(..))
import Texts
import Ticker.Announcement as Announcement exposing (Announcement)
import Ticker.Message as Message exposing (Message)
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



-- MODIFICATION


tick : Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
tick seed words game =
    checkAnnouncementDone seed words <|
        case game of
            Hotseat turn ->
                case turn of
                    GameStart ann ->
                        Hotseat (GameStart (ann |> Announcement.tick))

                    Rules ann ->
                        Hotseat (Rules (ann |> Announcement.tick))

                    RoundStart score athlete cnts ann ->
                        Hotseat (RoundStart score athlete cnts (ann |> Announcement.tick))

                    PlayCorrect score athlete cnts ann ->
                        Hotseat (PlayCorrect score athlete cnts (ann |> Announcement.tick))

                    PlayWrong score athlete cnts ann ->
                        Hotseat (PlayWrong score athlete cnts (ann |> Announcement.tick))

                    RoundEnd score athlete ann ->
                        Hotseat (RoundEnd score athlete (ann |> Announcement.tick))

                    Tally score athlete ann ->
                        Hotseat (Tally score athlete (ann |> Announcement.tick))

                    Assessment score athlete ann ->
                        Hotseat (Assessment score athlete (ann |> Announcement.tick))

                    NewRound score athlete ann ->
                        Hotseat (NewRound score athlete (ann |> Announcement.tick))

                    End athlete points ann ->
                        Hotseat (End athlete points (ann |> Announcement.tick))

                    Play _ _ _ _ ->
                        game

            Single turn ->
                Debug.todo "Single mode not implemented."


skip : Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
skip seed words game =
    let
        ignore =
            ( game, seed, Nothing )
    in
    case game of
        Hotseat turn ->
            case turn of
                RoundStart _ _ _ _ ->
                    ignore

                PlayCorrect _ _ _ _ ->
                    ignore

                PlayWrong _ _ _ _ ->
                    ignore

                Play _ _ _ _ ->
                    ignore

                _ ->
                    let
                        ( newGame, newSeed ) =
                            nextStatus seed words game

                        message =
                            getAnnouncement game
                                |> Maybe.map Announcement.toMessage
                    in
                    ( newGame
                    , newSeed
                    , message
                    )

        Single turn ->
            Debug.todo "Single mode not implemented."



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


checkAnnouncementDone : Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
checkAnnouncementDone seed words game =
    case getAnnouncement game of
        Just ann ->
            if Announcement.isFinished ann then
                let
                    ( newGame, newSeed ) =
                        nextStatus seed words game
                in
                ( newGame
                , newSeed
                , Just (Announcement.toMessage ann)
                )

            else
                ( game, seed, Nothing )

        Nothing ->
            ( game, seed, Nothing )


nextStatus : Random.Seed -> Words -> Game -> ( Game, Random.Seed )
nextStatus seed words game =
    case game of
        Hotseat turn ->
            case turn of
                GameStart _ ->
                    ( showRules, seed )

                Rules _ ->
                    startRound
                        { score = Score.emptyPlayingScore
                        , athlete = AthleteB
                        , seed = seed
                        }

                RoundStart score athlete cnts _ ->
                    ( startPlay
                        { score = score
                        , athlete = athlete
                        , constraints = cnts
                        }
                    , seed
                    )

                Play score athlete input cnts ->
                    athleteInputDone
                        { input = input
                        , words = words
                        , score = score
                        , athlete = athlete
                        , constraints = cnts
                        , seed = seed
                        }
                        |> Maybe.withDefault
                            ( game, seed )

                PlayCorrect score athlete cnts _ ->
                    ( startPlay
                        { score = score
                        , athlete = Utils.oppositeAthlete athlete
                        , constraints = cnts
                        }
                    , seed
                    )

                PlayWrong score athlete _ _ ->
                    endRound
                        { winner = Utils.oppositeAthlete athlete
                        , score = score
                        , seed = seed
                        }

                RoundEnd score athlete ann ->
                    case score of
                        PlayingScore playingScore ->
                            tally
                                { score = playingScore
                                , athlete = athlete
                                , seed = seed
                                }

                        WinnerScore winner loserScore ->
                            ( game, seed )

                Tally score athlete _ ->
                    assessment
                        { score = score
                        , athlete = athlete
                        , seed = seed
                        }

                Assessment score athlete _ ->
                    newRound
                        { score = score
                        , athlete = Utils.oppositeAthlete athlete
                        , seed = seed
                        }

                NewRound score athlete _ ->
                    startRound
                        { score = score
                        , athlete = athlete
                        , seed = seed
                        }

                End winner loserPoints _ ->
                    ( game, seed )

        Single turn ->
            Debug.todo "Single mode not implemented."


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
