module Game exposing
    ( Game
    , GameMode(..)
    , getActive
    , getActiveAthlete
    , skip
    , startGame
    , tick
    , userInput
    )

import Athlete exposing (..)
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
    | PlayCorrect GameMode PlayingScore Athlete Constraints Queue
    | PlayWrong GameMode Score Athlete Constraints Queue
    | RoundEnd GameMode Score Athlete Queue
    | Assessment GameMode PlayingScore Athlete Queue
    | End GameMode Athlete Points Queue


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


getActive : Game -> Active
getActive game =
    case game of
        GameStart _ queue ->
            Active.fromQueue queue

        RoundStart _ _ _ _ queue ->
            Active.fromQueue queue

        PlayCorrect _ _ _ _ queue ->
            Active.fromQueue queue

        PlayWrong _ _ _ _ queue ->
            Active.fromQueue queue

        RoundEnd _ _ _ queue ->
            Active.fromQueue queue

        Assessment _ _ _ queue ->
            Active.fromQueue queue

        End _ _ _ queue ->
            Active.fromQueue queue

        Play _ _ athlete input _ ->
            Active.athleteInput athlete input


getActiveAthlete : Game -> Maybe Athlete
getActiveAthlete game =
    case game of
        Play _ _ athlete _ _ ->
            Just athlete

        _ ->
            Nothing



-- MODIFICATION


tick : Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
tick seed words game =
    checkAnnouncementDone seed words <|
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

            Play _ _ _ _ _ ->
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
                    Nothing ->
                        nextStatus seed words game

                    Just newQueue ->
                        ( createGame newQueue
                        , seed
                        , Just (Announcement.toMessage ann)
                        )

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

        Play HotseatMode _ _ _ _ ->
            nextStatus seed words game

        Play SingleMode _ _ _ _ ->
            Debug.todo "Single mode not implemented yet."


userInput : String -> Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
userInput input seed words game =
    case game of
        Play HotseatMode score athlete previousInput cnts ->
            let
                newInput =
                    previousInput ++ input

                ( newGame, newSeed ) =
                    athleteInput
                        { input = newInput
                        , score = score
                        , athlete = athlete
                        , constraints = cnts
                        , words = words
                        , mode = HotseatMode
                        , seed = seed
                        }

                message =
                    case newGame of
                        PlayCorrect _ _ _ _ _ ->
                            Just (Message.CorrectAthleteInput athlete newInput)

                        PlayWrong _ _ _ _ _ ->
                            Just (Message.WrongAthleteInput athlete newInput)

                        _ ->
                            Nothing
            in
            ( newGame, newSeed, message )

        Play SingleMode _ _ _ _ ->
            Debug.todo "Single mode not implemented."

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


startPlay : { score : PlayingScore, athlete : Athlete, constraints : Constraints, mode : GameMode } -> Game
startPlay { score, athlete, mode, constraints } =
    Play mode score athlete "" constraints


athleteInput : { input : String, words : Words, score : PlayingScore, athlete : Athlete, constraints : Constraints, mode : GameMode, seed : Random.Seed } -> ( Game, Random.Seed )
athleteInput { input, words, score, athlete, constraints, mode, seed } =
    let
        playWrongWith messageFn =
            playWrong
                { messageFn = messageFn
                , score = score
                , athlete = athlete
                , constraints = constraints
                , mode = mode
                , seed = seed
                }
    in
    case Constraints.checkCandidate input constraints words of
        Constraints.CandidateCorrect ->
            ( Play mode score athlete input constraints
            , seed
            )

        Constraints.CandidateInitialWrong ->
            playWrongWith Texts.initialWrong

        Constraints.CandidateNotAWord ->
            playWrongWith Texts.notAWord


athleteInputDone : { input : String, words : Words, constraints : Constraints, score : PlayingScore, athlete : Athlete, mode : GameMode, seed : Random.Seed } -> ( Game, Random.Seed, Maybe Message )
athleteInputDone { input, words, constraints, score, athlete, mode, seed } =
    let
        playWrongWith messageFn =
            playWrong
                { messageFn = messageFn
                , score = score
                , athlete = athlete
                , constraints = constraints
                , mode = mode
                , seed = seed
                }

        addMessage messageConstructor ( g, s ) =
            ( g, s, Just (messageConstructor athlete input) )
    in
    if String.length input > 0 then
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
                    |> addMessage Message.WrongAthleteInput

            Constraints.InputIncorporatesWrong ->
                playWrongWith Texts.incorporatesWrong
                    |> addMessage Message.WrongAthleteInput

            Constraints.InputAlreadyPlayed ->
                playWrongWith Texts.alreadyPlayed
                    |> addMessage Message.WrongAthleteInput

            Constraints.InputNotAWord ->
                playWrongWith Texts.notAWord
                    |> addMessage Message.WrongAthleteInput

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


getQueue : Game -> Maybe Queue
getQueue game =
    case game of
        GameStart _ queue ->
            Just queue

        RoundStart _ _ _ _ queue ->
            Just queue

        PlayCorrect _ _ _ _ queue ->
            Just queue

        PlayWrong _ _ _ _ queue ->
            Just queue

        RoundEnd _ _ _ queue ->
            Just queue

        Assessment _ _ _ queue ->
            Just queue

        End _ _ _ queue ->
            Just queue

        Play _ _ _ _ _ ->
            Nothing


checkAnnouncementDone : Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
checkAnnouncementDone seed words game =
    let
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
                ( game, seed, Nothing )
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
            ( game, seed, Nothing )


nextStatus : Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
nextStatus seed words game =
    let
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
            ( startPlay
                { score = score
                , athlete = athlete
                , constraints = cnts
                , mode = mode
                }
            , seed
            )
                |> addMessage queue

        Play mode score athlete input cnts ->
            case mode of
                HotseatMode ->
                    athleteInputDone
                        { input = input
                        , words = words
                        , score = score
                        , athlete = athlete
                        , constraints = cnts
                        , mode = mode
                        , seed = seed
                        }

                SingleMode ->
                    Debug.todo "Single mode not implemented."

        PlayCorrect mode score athlete cnts queue ->
            ( startPlay
                { score = score
                , athlete = Utils.oppositeAthlete athlete
                , constraints = cnts
                , mode = mode
                }
            , seed
            )
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

        End mode winner loserPoints queue ->
            ( game, seed, Nothing )


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
