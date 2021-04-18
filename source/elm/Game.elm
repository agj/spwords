module Game exposing
    ( Game
    , GameMode(..)
    , ended
    , getActive
    , getActiveAthlete
    , skip
    , startGame
    , tick
    , userInput
    )

import Athlete exposing (..)
import ComputerThought exposing (ComputerThought)
import Constraints exposing (Constraints)
import Doc.Format
import Doc.Paragraph exposing (Paragraph)
import Doc.Text
import Random
import Score exposing (PlayingScore, Points, Score(..))
import Texts exposing (newRound)
import Ticker.Active as Active exposing (Active)
import Ticker.Announcement as Announcement exposing (Announcement)
import Ticker.Message as Message exposing (Message)
import Ticker.Queue as Queue exposing (Queue)
import Utils
import Words exposing (Words)


type Game
    = GameStart GameMode Queue
    | RoundStart GameMode PlayingScore Athlete Constraints Queue
    | Play GameMode PlayingScore Athlete String Constraints
    | ComputerPlay PlayingScore ComputerThought Constraints
    | PlayCorrect GameMode PlayingScore Athlete Constraints Queue
    | PlayWrong GameMode Score Athlete Constraints Queue
    | RoundEnd GameMode Score Athlete Queue
    | Assessment GameMode PlayingScore Athlete Queue
    | End GameMode Athlete Points Queue
    | Done


type GameMode
    = HotseatMode
    | SingleMode


startGame : GameMode -> Game
startGame mode =
    GameStart
        mode
        (Queue.fromList
            (Texts.gameStart
                { athleteA = "left"
                , athleteB = "right"
                }
                |> Announcement.create
            )
            [ Texts.rules |> Announcement.create ]
        )


getActive : Game -> Maybe Active
getActive game =
    case game of
        GameStart _ queue ->
            Just (Active.fromQueue queue)

        RoundStart _ _ _ _ queue ->
            Just (Active.fromQueue queue)

        PlayCorrect _ _ _ _ queue ->
            Just (Active.fromQueue queue)

        PlayWrong _ _ _ _ queue ->
            Just (Active.fromQueue queue)

        RoundEnd _ _ _ queue ->
            Just (Active.fromQueue queue)

        Assessment _ _ _ queue ->
            Just (Active.fromQueue queue)

        End _ _ _ queue ->
            Just (Active.fromQueue queue)

        Play _ _ athlete input _ ->
            Just (Active.athleteInput athlete input)

        ComputerPlay _ thought _ ->
            Just (Active.athleteInput AthleteB (ComputerThought.getInput thought))

        Done ->
            Nothing


getActiveAthlete : Game -> Maybe Athlete
getActiveAthlete game =
    case game of
        Play _ _ athlete _ _ ->
            Just athlete

        ComputerPlay _ _ _ ->
            Just AthleteB

        _ ->
            Nothing


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

            RoundStart mode score athlete cnts queue ->
                RoundStart mode score athlete cnts (queue |> Queue.tick)

            PlayCorrect mode score athlete cnts queue ->
                PlayCorrect mode score athlete cnts (queue |> Queue.tick)

            PlayWrong mode score athlete cnts queue ->
                PlayWrong mode score athlete cnts (queue |> Queue.tick)

            RoundEnd mode score athlete queue ->
                RoundEnd mode score athlete (queue |> Queue.tick)

            Assessment mode score athlete queue ->
                Assessment mode score athlete (queue |> Queue.tick)

            End mode athlete points queue ->
                End mode athlete points (queue |> Queue.tick)

            ComputerPlay score thought cnts ->
                ComputerPlay score (ComputerThought.tick thought) cnts

            Play _ _ _ _ _ ->
                game

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

        RoundStart mode score athlete cnts queue ->
            check queue (\newQueue -> RoundStart mode score athlete cnts newQueue)

        PlayCorrect mode score athlete cnts queue ->
            check queue (\newQueue -> PlayCorrect mode score athlete cnts newQueue)

        PlayWrong mode score athlete cnts queue ->
            check queue (\newQueue -> PlayWrong mode score athlete cnts newQueue)

        RoundEnd mode score athlete queue ->
            check queue (\newQueue -> RoundEnd mode score athlete newQueue)

        Assessment mode score athlete queue ->
            check queue (\newQueue -> Assessment mode score athlete newQueue)

        End mode athlete points queue ->
            check queue (\newQueue -> End mode athlete points newQueue)

        Play _ _ _ _ _ ->
            nextStatus seed words game

        ComputerPlay _ _ _ ->
            ignore

        Done ->
            ignore


userInput : String -> Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
userInput input seed words game =
    case game of
        Play mode score athlete previousInput cnts ->
            let
                newInput =
                    previousInput ++ input

                playWrongWith messageFn =
                    let
                        ( g, s ) =
                            playWrong
                                { messageFn = messageFn
                                , score = score
                                , athlete = athlete
                                , constraints = cnts
                                , mode = mode
                                , seed = seed
                                }
                    in
                    ( g, s, Just (Message.WrongAthleteInput athlete newInput) )
            in
            case Constraints.checkCandidate newInput cnts words of
                Constraints.CandidateCorrect ->
                    ( Play mode score athlete newInput cnts
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


startRound : { athlete : Athlete, score : PlayingScore, mode : GameMode, seed : Random.Seed } -> ( Game, Random.Seed )
startRound { athlete, score, mode, seed } =
    let
        ( initial, seed1 ) =
            randomLetter seed Texts.alphabet

        ( roundStartMsg, newSeed ) =
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

        ann =
            roundStartMsg |> Announcement.createUnskippable

        newGame =
            RoundStart
                mode
                score
                athlete
                (Constraints.serve initial)
                (Queue.singleton ann)
    in
    ( newGame
    , newSeed
    )


startPlay : { score : PlayingScore, athlete : Athlete, constraints : Constraints, words : Words, mode : GameMode, seed : Random.Seed } -> ( Game, Random.Seed )
startPlay { score, athlete, mode, constraints, words, seed } =
    case mode of
        HotseatMode ->
            ( Play mode score athlete "" constraints
            , seed
            )

        SingleMode ->
            case athlete of
                AthleteA ->
                    ( Play mode score athlete "" constraints
                    , seed
                    )

                AthleteB ->
                    let
                        ( thought, newSeed ) =
                            ComputerThought.create words constraints seed
                    in
                    ( ComputerPlay score thought constraints
                    , newSeed
                    )


checkComputerCandidate : Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
checkComputerCandidate seed words game =
    let
        ignore =
            ( game, seed, Nothing )
    in
    case game of
        ComputerPlay score thought cnts ->
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


athleteInputDone : { input : String, words : Words, constraints : Constraints, score : PlayingScore, athlete : Athlete, mode : GameMode, seed : Random.Seed } -> ( Game, Random.Seed, Maybe Message )
athleteInputDone { input, words, constraints, score, athlete, mode, seed } =
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
        ( Play mode score athlete input constraints
        , seed
        , Nothing
        )


endRound : { winner : Athlete, score : Score, mode : GameMode, seed : Random.Seed } -> ( Game, Random.Seed )
endRound { winner, score, mode, seed } =
    let
        ( message, newSeed ) =
            Texts.roundEnd
                { winner = winner
                , athleteA = "left"
                , athleteB = "right"
                , seed = seed
                }

        newGame =
            RoundEnd mode score winner (Queue.singleton (message |> Announcement.create))
    in
    ( newGame, newSeed )


assessment : { score : PlayingScore, athlete : Athlete, mode : GameMode, seed : Random.Seed } -> ( Game, Random.Seed )
assessment { score, athlete, mode, seed } =
    let
        ( tallyMsg, seed1 ) =
            Texts.tally
                { athleteA = "left"
                , athleteB = "right"
                , pointsA = Tuple.first score
                , pointsB = Tuple.second score
                , seed = seed
                }

        ( comparisonMsg, seed2 ) =
            case score of
                ( pointsA, pointsB ) ->
                    if pointsA == pointsB then
                        Texts.tie
                            { points = pointsA
                            , seed = seed1
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
                            , seed = seed1
                            }

        ( newRoundMsg, newSeed ) =
            Texts.newRound seed2

        ( ann, anns ) =
            ( tallyMsg |> Announcement.create
            , [ comparisonMsg |> Announcement.create
              , newRoundMsg |> Announcement.create
              ]
            )

        newGame =
            Assessment mode score athlete (Queue.fromList ann anns)
    in
    ( newGame, newSeed )


endGame : { winner : Athlete, loserPoints : Points, mode : GameMode } -> Game
endGame { winner, loserPoints, mode } =
    let
        message =
            Texts.gameEnd
                { winner = winner
                , loserPoints = loserPoints
                , athleteA = "left"
                , athleteB = "right"
                }
                |> Announcement.createUnskippable
    in
    End mode winner loserPoints (Queue.singleton message)



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

        RoundStart mode score athlete cnts queue ->
            check queue (\newQueue -> RoundStart mode score athlete cnts newQueue)

        PlayCorrect mode score athlete cnts queue ->
            check queue (\newQueue -> PlayCorrect mode score athlete cnts newQueue)

        PlayWrong mode score athlete cnts queue ->
            check queue (\newQueue -> PlayWrong mode score athlete cnts newQueue)

        RoundEnd mode score athlete queue ->
            check queue (\newQueue -> RoundEnd mode score athlete newQueue)

        Assessment mode score athlete queue ->
            check queue (\newQueue -> Assessment mode score athlete newQueue)

        End mode athlete points queue ->
            check queue (\newQueue -> End mode athlete points newQueue)

        ComputerPlay _ thought _ ->
            if ComputerThought.isFinished thought then
                nextStatus seed words game

            else
                checkComputerCandidate seed words game

        Play _ _ _ _ _ ->
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
                , athlete = AthleteB
                , mode = mode
                , seed = seed
                }
                |> addMessage queue

        RoundStart mode score athlete cnts queue ->
            startPlay
                { score = score
                , athlete = athlete
                , constraints = cnts
                , words = words
                , mode = mode
                , seed = seed
                }
                |> addMessage queue

        Play mode score athlete input cnts ->
            athleteInputDone
                { input = input
                , words = words
                , score = score
                , athlete = athlete
                , constraints = cnts
                , mode = mode
                , seed = seed
                }

        ComputerPlay score thought cnts ->
            athleteInputDone
                { input = ComputerThought.getInput thought
                , words = words
                , score = score
                , athlete = AthleteB
                , constraints = cnts
                , mode = SingleMode
                , seed = seed
                }

        PlayCorrect mode score athlete cnts queue ->
            startPlay
                { score = score
                , athlete = Utils.oppositeAthlete athlete
                , constraints = cnts
                , words = words
                , mode = mode
                , seed = seed
                }
                |> addMessage queue

        PlayWrong mode score athlete _ queue ->
            endRound
                { winner = Utils.oppositeAthlete athlete
                , score = score
                , mode = mode
                , seed = seed
                }
                |> addMessage queue

        RoundEnd mode score athlete queue ->
            case score of
                PlayingScore playingScore ->
                    assessment
                        { score = playingScore
                        , athlete = athlete
                        , mode = mode
                        , seed = seed
                        }
                        |> addMessage queue

                WinnerScore winner loserPoints ->
                    ( endGame
                        { winner = winner
                        , loserPoints = loserPoints
                        , mode = mode
                        }
                    , seed
                    )
                        |> addMessage queue

        Assessment mode score athlete queue ->
            startRound
                { score = score
                , athlete = Utils.oppositeAthlete athlete
                , mode = mode
                , seed = seed
                }
                |> addMessage queue

        End _ _ _ queue ->
            ( Done, seed, Just (queue |> Queue.peek |> Announcement.toMessage) )

        Done ->
            ignore


playCorrect : { constraints : Constraints, score : PlayingScore, athlete : Athlete, mode : GameMode, seed : Random.Seed } -> ( Game, Random.Seed )
playCorrect { constraints, score, athlete, mode, seed } =
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
            PlayCorrect
                mode
                score
                athlete
                newCnts
                (Queue.singleton (message |> Announcement.createUnskippable))
    in
    ( newGame, newSeed )


playWrong : { messageFn : Texts.MistakeArguments -> ( Paragraph, Random.Seed ), score : PlayingScore, mode : GameMode, athlete : Athlete, constraints : Constraints, seed : Random.Seed } -> ( Game, Random.Seed )
playWrong { messageFn, score, athlete, constraints, mode, seed } =
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
            PlayWrong
                mode
                newScore
                athlete
                constraints
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
    Utils.stringCharAt n alpha
        |> Maybe.withDefault '?'


isComputerAthlete : GameMode -> Athlete -> Bool
isComputerAthlete mode athlete =
    mode == SingleMode && athlete == AthleteB
