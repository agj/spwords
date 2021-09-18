module Game exposing
    ( Game
    , ended
    , getActive
    , getActiveAthlete
    , getTimes
    , skip
    , startGame
    , tick
    , userInput
    )

import Athlete exposing (..)
import ComputerThought exposing (ComputerThought)
import Constraints exposing (Constraints)
import Doc.Paragraph exposing (Paragraph)
import Game.GameMode exposing (GameMode(..))
import Game.Times as Times exposing (Times)
import Random
import Score exposing (PlayingScore, Points, Score(..))
import Texts
import Ticker.Active as Active exposing (Active)
import Ticker.Announcement as Announcement exposing (Announcement)
import Ticker.Message as Message exposing (Message)
import Ticker.Queue as Queue exposing (Queue)
import Util.String as String
import Words exposing (Words)


type Game
    = GameStart GameMode Queue
    | RoundStart GameMode PlayingScore Athlete Constraints Times Queue
    | Play GameMode PlayingScore Athlete String Constraints Times
    | ComputerPlay PlayingScore ComputerThought Constraints Times
    | PlayCorrect GameMode PlayingScore Athlete Constraints Times Queue
    | PlayWrong GameMode Score Athlete Constraints Times Queue
    | RoundEnd GameMode Score Athlete Played Queue
    | Assessment GameMode PlayingScore Athlete Played Queue
    | End GameMode Athlete Points Queue
    | Done


type alias Played =
    List String


startGame : GameMode -> Game
startGame mode =
    GameStart
        mode
        (Queue.fromList
            (Texts.gameStart mode
                |> Announcement.create
            )
            [ Texts.rules |> Announcement.create ]
        )


getActive : Game -> Maybe Active
getActive game =
    case game of
        GameStart _ queue ->
            Just (Active.fromQueue queue)

        RoundStart _ _ _ _ _ queue ->
            Just (Active.fromQueue queue)

        PlayCorrect _ _ _ _ _ queue ->
            Just (Active.fromQueue queue)

        PlayWrong _ _ _ _ _ queue ->
            Just (Active.fromQueue queue)

        RoundEnd _ _ _ _ queue ->
            Just (Active.fromQueue queue)

        Assessment _ _ _ _ queue ->
            Just (Active.fromQueue queue)

        End _ _ _ queue ->
            Just (Active.fromQueue queue)

        Play _ _ athlete input _ _ ->
            Just (Active.athleteInput athlete input)

        ComputerPlay _ thought _ _ ->
            Just (Active.athleteInput AthleteB (ComputerThought.getInput thought))

        Done ->
            Nothing


getActiveAthlete : Game -> Maybe Athlete
getActiveAthlete game =
    case game of
        Play _ _ athlete _ _ _ ->
            Just athlete

        ComputerPlay _ _ _ _ ->
            Just AthleteB

        _ ->
            Nothing


getTimes : Game -> Times
getTimes game =
    case game of
        GameStart _ _ ->
            Times.start

        RoundStart _ _ _ _ times _ ->
            times

        PlayCorrect _ _ _ _ times _ ->
            times

        PlayWrong _ _ _ _ times _ ->
            times

        Play _ _ _ _ _ times ->
            times

        ComputerPlay _ _ _ times ->
            times

        RoundEnd _ _ _ _ _ ->
            Times.start

        Assessment _ _ _ _ _ ->
            Times.start

        End _ _ _ _ ->
            Times.start

        Done ->
            Times.start


ended : Game -> Bool
ended game =
    case game of
        Done ->
            True

        _ ->
            False



-- MODIFICATION


tick : Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
tick seed words game =
    checkDone seed words <|
        case game of
            GameStart mode queue ->
                GameStart mode (queue |> Queue.tick)

            RoundStart mode score athlete cnts times queue ->
                RoundStart mode score athlete cnts times (queue |> Queue.tick)

            PlayCorrect mode score athlete cnts times queue ->
                PlayCorrect mode score athlete cnts times (queue |> Queue.tick)

            PlayWrong mode score athlete cnts times queue ->
                PlayWrong mode score athlete cnts times (queue |> Queue.tick)

            RoundEnd mode score athlete played queue ->
                RoundEnd mode score athlete played (queue |> Queue.tick)

            Assessment mode score athlete played queue ->
                Assessment mode score athlete played (queue |> Queue.tick)

            End mode athlete points queue ->
                End mode athlete points (queue |> Queue.tick)

            ComputerPlay score thought cnts times ->
                ComputerPlay score (ComputerThought.tick thought) cnts (Times.tick AthleteB times)

            Play mode score athlete input cnts times ->
                Play mode score athlete input cnts (Times.tick athlete times)

            Done ->
                game


skip : Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
skip seed words game =
    let
        ignore =
            ( game, seed, Nothing )

        check queue createGame =
            let
                ( ann, newQueueM ) =
                    Queue.pop queue
            in
            if Announcement.isSkippable ann then
                case newQueueM of
                    Just newQueue ->
                        ( createGame newQueue
                        , seed
                        , Just (Announcement.toMessage ann)
                        )

                    Nothing ->
                        nextStatus seed words game

            else
                ignore
    in
    case game of
        GameStart mode queue ->
            check queue (\newQueue -> GameStart mode newQueue)

        RoundStart mode score athlete cnts times queue ->
            check queue (\newQueue -> RoundStart mode score athlete cnts times newQueue)

        PlayCorrect mode score athlete cnts times queue ->
            check queue (\newQueue -> PlayCorrect mode score athlete cnts times newQueue)

        PlayWrong mode score athlete cnts times queue ->
            check queue (\newQueue -> PlayWrong mode score athlete cnts times newQueue)

        RoundEnd mode score athlete played queue ->
            check queue (\newQueue -> RoundEnd mode score athlete played newQueue)

        Assessment mode score athlete played queue ->
            check queue (\newQueue -> Assessment mode score athlete played newQueue)

        End mode athlete points queue ->
            check queue (\newQueue -> End mode athlete points newQueue)

        Play _ _ _ _ _ _ ->
            nextStatus seed words game

        ComputerPlay _ _ _ _ ->
            ignore

        Done ->
            ignore


userInput : String -> Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
userInput input seed words game =
    case game of
        Play mode score athlete previousInput cnts times ->
            let
                fixedInput =
                    String.filter (\ch -> String.member ch Texts.alphabet) input

                newInput =
                    previousInput ++ fixedInput

                playWrongWith messageFn =
                    let
                        ( g, s ) =
                            playWrong
                                { messageFn = messageFn
                                , score = score
                                , athlete = athlete
                                , constraints = cnts
                                , mode = mode
                                , times = times
                                , seed = seed
                                }
                    in
                    ( g, s, Just (Message.WrongAthleteInput athlete newInput) )
            in
            case Constraints.checkCandidate newInput cnts words of
                Constraints.CandidateCorrect ->
                    ( Play mode score athlete newInput cnts times
                    , seed
                    , Nothing
                    )

                Constraints.CandidateInitialWrong ->
                    playWrongWith Texts.initialWrong

                Constraints.CandidateNotAWord ->
                    playWrongWith Texts.notAWord

        _ ->
            ( game, seed, Nothing )



--------------- INTERNAL ---------------
--
-- GAME TURN GENERATION


startRound : { athlete : Athlete, score : PlayingScore, played : Played, mode : GameMode, seed : Random.Seed } -> ( Game, Random.Seed )
startRound { athlete, score, played, mode, seed } =
    let
        ( initial, seed1 ) =
            randomLetter seed Texts.alphabet

        ( roundStartMsg, newSeed ) =
            Random.step
                (Texts.roundStart
                    { turnAthlete = athlete
                    , mode = mode
                    , initial = initial
                    }
                )
                seed1

        ann =
            roundStartMsg |> Announcement.createUnskippable

        newGame =
            RoundStart
                mode
                score
                athlete
                (Constraints.serve
                    { initial = initial
                    , played = played
                    }
                )
                Times.start
                (Queue.singleton ann)
    in
    ( newGame
    , newSeed
    )


startPlay : { score : PlayingScore, athlete : Athlete, constraints : Constraints, words : Words, mode : GameMode, times : Times, seed : Random.Seed } -> ( Game, Random.Seed )
startPlay { score, athlete, mode, constraints, words, times, seed } =
    let
        userPlay =
            ( Play mode score athlete "" constraints times
            , seed
            )
    in
    case mode of
        HotseatMode ->
            userPlay

        SingleMode ->
            case athlete of
                AthleteA ->
                    userPlay

                AthleteB ->
                    let
                        ( thought, newSeed ) =
                            ComputerThought.create words constraints seed
                    in
                    ( ComputerPlay score thought constraints times
                    , newSeed
                    )


checkComputerCandidate : Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
checkComputerCandidate seed words game =
    let
        ignore =
            ( game, seed, Nothing )
    in
    case game of
        ComputerPlay score thought cnts times ->
            let
                input =
                    ComputerThought.getInput thought

                playWrongWith messageFn =
                    let
                        ( g, s ) =
                            playWrong
                                { messageFn = messageFn
                                , score = score
                                , athlete = AthleteB
                                , constraints = cnts
                                , mode = SingleMode
                                , times = times
                                , seed = seed
                                }
                    in
                    ( g, s, Just (Message.WrongAthleteInput AthleteB input) )
            in
            case Constraints.checkCandidate input cnts words of
                Constraints.CandidateCorrect ->
                    ignore

                Constraints.CandidateInitialWrong ->
                    playWrongWith Texts.initialWrong

                Constraints.CandidateNotAWord ->
                    playWrongWith Texts.notAWord

        _ ->
            ignore


athleteInputDone : { input : String, words : Words, constraints : Constraints, score : PlayingScore, athlete : Athlete, mode : GameMode, times : Times, seed : Random.Seed } -> ( Game, Random.Seed, Maybe Message )
athleteInputDone { input, words, constraints, score, athlete, mode, times, seed } =
    let
        playWrongWith messageFn =
            let
                ( g, s ) =
                    playWrong
                        { messageFn = messageFn
                        , score = score
                        , athlete = athlete
                        , constraints = constraints
                        , mode = mode
                        , times = times
                        , seed = seed
                        }
            in
            ( g, s, Just (Message.WrongAthleteInput athlete input) )

        addMessage messageConstructor ( g, s ) =
            ( g, s, Just (messageConstructor athlete input) )
    in
    if String.length input > 0 || isComputerAthlete mode athlete then
        case Constraints.check input constraints words of
            Constraints.InputCorrect ->
                let
                    newCnts =
                        constraints |> Constraints.pushPlayed input
                in
                playCorrect
                    { constraints = newCnts
                    , score = score
                    , athlete = athlete
                    , mode = mode
                    , times = times
                    , seed = seed
                    }
                    |> addMessage Message.CorrectAthleteInput

            Constraints.InputInitialWrong ->
                playWrongWith Texts.initialWrong

            Constraints.InputIncorporatesWrong ->
                playWrongWith Texts.incorporatesWrong

            Constraints.InputAlreadyPlayed ->
                playWrongWith Texts.alreadyPlayed

            Constraints.InputNotAWord ->
                playWrongWith Texts.notAWord

    else
        ( Play mode score athlete input constraints times
        , seed
        , Nothing
        )


timeUp : { input : String, score : PlayingScore, athlete : Athlete, mode : GameMode, times : Times, seed : Random.Seed } -> ( Game, Random.Seed, Maybe Message )
timeUp { input, score, athlete, mode, times, seed } =
    let
        ( newGame, newSeed ) =
            playWrong
                { messageFn = Texts.timeUp
                , score = score
                , athlete = athlete
                , constraints = Constraints.serve { initial = '?', played = [] }
                , mode = mode
                , times = times
                , seed = seed
                }
    in
    ( newGame, newSeed, Just (Message.WrongAthleteInput athlete input) )


endRound : { winner : Athlete, score : Score, mode : GameMode, played : Played, seed : Random.Seed } -> ( Game, Random.Seed )
endRound { winner, score, played, mode, seed } =
    let
        ( message, newSeed ) =
            Texts.roundEnd
                { winner = winner
                , mode = mode
                , seed = seed
                }

        newGame =
            RoundEnd mode score winner played (Queue.singleton (message |> Announcement.create))
    in
    ( newGame, newSeed )


assessment : { score : PlayingScore, athlete : Athlete, played : Played, mode : GameMode, seed : Random.Seed } -> ( Game, Random.Seed )
assessment { score, athlete, played, mode, seed } =
    let
        ( pointsA, pointsB ) =
            score

        ( newRoundMsg, seed1 ) =
            Texts.newRound seed

        ( ann, anns, newSeed ) =
            if pointsA /= pointsB then
                let
                    ( tallyMsg, s1 ) =
                        Texts.tally
                            { mode = mode
                            , pointsA = pointsA
                            , pointsB = pointsB
                            , seed = seed1
                            }

                    ( assessmentMsg, s2 ) =
                        Texts.assessment
                            { winner =
                                if (pointsA |> Score.intFromPoints) > (pointsB |> Score.intFromPoints) then
                                    AthleteA

                                else
                                    AthleteB
                            , mode = mode
                            , seed = s1
                            }
                in
                ( tallyMsg |> Announcement.create
                , [ assessmentMsg |> Announcement.create
                  , newRoundMsg |> Announcement.create
                  ]
                , s2
                )

            else
                let
                    ( tiedMsg, s1 ) =
                        Texts.tallyAssessmentTied
                            { points = pointsA
                            , seed = seed1
                            }
                in
                ( tiedMsg |> Announcement.create
                , [ newRoundMsg |> Announcement.create ]
                , s1
                )

        newGame =
            Assessment mode score athlete played (Queue.fromList ann anns)
    in
    ( newGame, newSeed )


endGame : { winner : Athlete, loserPoints : Points, mode : GameMode, seed : Random.Seed } -> ( Game, Random.Seed )
endGame { winner, loserPoints, mode, seed } =
    let
        ( message, newSeed ) =
            Texts.gameEnd
                { winner = winner
                , loserPoints = loserPoints
                , mode = mode
                , seed = seed
                }
                |> Tuple.mapFirst Announcement.createUnskippable
    in
    ( End mode winner loserPoints (Queue.singleton message)
    , newSeed
    )



-- OTHER


checkDone : Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
checkDone seed words game =
    let
        ignore =
            ( game, seed, Nothing )

        check queue gameCreator =
            let
                ( ann, poppedQueueM ) =
                    Queue.pop queue
            in
            if Announcement.isFinished ann then
                case poppedQueueM of
                    Nothing ->
                        nextStatus seed words game

                    Just newQueue ->
                        ( gameCreator newQueue, seed, Just (Announcement.toMessage ann) )

            else
                ignore
    in
    case game of
        GameStart mode queue ->
            check queue (\newQueue -> GameStart mode newQueue)

        RoundStart mode score athlete cnts times queue ->
            check queue (\newQueue -> RoundStart mode score athlete cnts times newQueue)

        PlayCorrect mode score athlete cnts times queue ->
            check queue (\newQueue -> PlayCorrect mode score athlete cnts times newQueue)

        PlayWrong mode score athlete cnts times queue ->
            check queue (\newQueue -> PlayWrong mode score athlete cnts times newQueue)

        RoundEnd mode score athlete played queue ->
            check queue (\newQueue -> RoundEnd mode score athlete played newQueue)

        Assessment mode score athlete played queue ->
            check queue (\newQueue -> Assessment mode score athlete played newQueue)

        End mode athlete points queue ->
            check queue (\newQueue -> End mode athlete points newQueue)

        ComputerPlay score thought _ times ->
            if ComputerThought.isFinished thought then
                nextStatus seed words game

            else if Times.isUp AthleteB times then
                timeUp
                    { athlete = AthleteB
                    , times = times
                    , mode = SingleMode
                    , score = score
                    , input = ComputerThought.getInput thought
                    , seed = seed
                    }

            else
                checkComputerCandidate seed words game

        Play mode score athlete input _ times ->
            if Times.isUp athlete times then
                timeUp
                    { athlete = athlete
                    , times = times
                    , mode = mode
                    , score = score
                    , input = input
                    , seed = seed
                    }

            else
                ignore

        Done ->
            ignore


nextStatus : Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
nextStatus seed words game =
    let
        ignore =
            ( game, seed, Nothing )

        addMessage queue ( g, s ) =
            ( g, s, Just (queue |> Queue.peek |> Announcement.toMessage) )
    in
    case game of
        GameStart mode queue ->
            startRound
                { score = Score.emptyPlayingScore
                , athlete =
                    if mode == HotseatMode then
                        AthleteA

                    else
                        AthleteB
                , played = []
                , mode = mode
                , seed = seed
                }
                |> addMessage queue

        RoundStart mode score athlete cnts times queue ->
            startPlay
                { score = score
                , athlete = athlete
                , constraints = cnts
                , words = words
                , mode = mode
                , times = times
                , seed = seed
                }
                |> addMessage queue

        Play mode score athlete input cnts times ->
            athleteInputDone
                { input = input
                , words = words
                , score = score
                , athlete = athlete
                , constraints = cnts
                , mode = mode
                , times = times
                , seed = seed
                }

        ComputerPlay score thought cnts times ->
            athleteInputDone
                { input = ComputerThought.getInput thought
                , words = words
                , score = score
                , athlete = AthleteB
                , constraints = cnts
                , mode = SingleMode
                , times = times
                , seed = seed
                }

        PlayCorrect mode score athlete cnts times queue ->
            startPlay
                { score = score
                , athlete = Athlete.opposite athlete
                , constraints = cnts
                , words = words
                , mode = mode
                , times = times
                , seed = seed
                }
                |> addMessage queue

        PlayWrong mode score athlete cnts _ queue ->
            endRound
                { winner = Athlete.opposite athlete
                , score = score
                , mode = mode
                , played = Constraints.getPlayed cnts
                , seed = seed
                }
                |> addMessage queue

        RoundEnd mode score athlete played queue ->
            case score of
                PlayingScore playingScore ->
                    assessment
                        { score = playingScore
                        , athlete = athlete
                        , played = played
                        , mode = mode
                        , seed = seed
                        }
                        |> addMessage queue

                WinnerScore winner loserPoints ->
                    endGame
                        { winner = winner
                        , loserPoints = loserPoints
                        , mode = mode
                        , seed = seed
                        }
                        |> addMessage queue

        Assessment mode score athlete played queue ->
            startRound
                { score = score
                , athlete = Athlete.opposite athlete
                , played = played
                , mode = mode
                , seed = seed
                }
                |> addMessage queue

        End _ _ _ queue ->
            ( Done, seed, Just (queue |> Queue.peek |> Announcement.toMessage) )

        Done ->
            ignore


playCorrect : { constraints : Constraints, score : PlayingScore, athlete : Athlete, mode : GameMode, times : Times, seed : Random.Seed } -> ( Game, Random.Seed )
playCorrect { constraints, score, athlete, mode, times, seed } =
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
            Random.step Texts.interjection seed

        newGame =
            PlayCorrect
                mode
                score
                athlete
                newCnts
                times
                (Queue.singleton (message |> Announcement.createUnskippable))
    in
    ( newGame, newSeed )


playWrong : { messageFn : Texts.MistakeArguments -> ( Paragraph, Random.Seed ), score : PlayingScore, mode : GameMode, athlete : Athlete, constraints : Constraints, times : Times, seed : Random.Seed } -> ( Game, Random.Seed )
playWrong { messageFn, score, athlete, constraints, mode, times, seed } =
    let
        newScore =
            Score.increaseScore (Athlete.opposite athlete) score

        ( message, newSeed ) =
            messageFn
                { initial = constraints |> Constraints.getInitial
                , incorporates = constraints |> Constraints.getIncorporates
                , seed = seed
                }

        newGame =
            PlayWrong
                mode
                newScore
                athlete
                constraints
                times
                (Queue.singleton (message |> Announcement.createUnskippable))
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
    String.charAt n alpha
        |> Maybe.withDefault '?'


isComputerAthlete : GameMode -> Athlete -> Bool
isComputerAthlete mode athlete =
    mode == SingleMode && athlete == AthleteB
