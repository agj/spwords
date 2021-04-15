module Game exposing
    ( Game
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
    = Hotseat Turn
    | Single Turn


type Turn
    = GameStart Queue
    | RoundStart PlayingScore Athlete Constraints Queue
    | Play PlayingScore Athlete String Constraints
    | PlayCorrect PlayingScore Athlete Constraints Queue
    | PlayWrong Score Athlete Constraints Queue
    | RoundEnd Score Athlete Queue
    | Assessment PlayingScore Athlete Queue
    | End Athlete Points Queue


startGame : Game
startGame =
    Hotseat
        (GameStart
            (Queue.fromList
                (Texts.gameStart
                    { athleteA = "left"
                    , athleteB = "right"
                    }
                    |> Announcement.create
                )
                [ Texts.rules |> Announcement.create ]
            )
        )


getActive : Game -> Active
getActive game =
    case game of
        Hotseat turn ->
            case turn of
                GameStart queue ->
                    Active.fromQueue queue

                RoundStart _ _ _ queue ->
                    Active.fromQueue queue

                PlayCorrect _ _ _ queue ->
                    Active.fromQueue queue

                PlayWrong _ _ _ queue ->
                    Active.fromQueue queue

                RoundEnd _ _ queue ->
                    Active.fromQueue queue

                Assessment _ _ queue ->
                    Active.fromQueue queue

                End _ _ queue ->
                    Active.fromQueue queue

                Play _ athlete input _ ->
                    Active.athleteInput athlete input

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
                    GameStart queue ->
                        Hotseat (GameStart (queue |> Queue.tick))

                    RoundStart score athlete cnts queue ->
                        Hotseat (RoundStart score athlete cnts (queue |> Queue.tick))

                    PlayCorrect score athlete cnts queue ->
                        Hotseat (PlayCorrect score athlete cnts (queue |> Queue.tick))

                    PlayWrong score athlete cnts queue ->
                        Hotseat (PlayWrong score athlete cnts (queue |> Queue.tick))

                    RoundEnd score athlete queue ->
                        Hotseat (RoundEnd score athlete (queue |> Queue.tick))

                    Assessment score athlete queue ->
                        Hotseat (Assessment score athlete (queue |> Queue.tick))

                    End athlete points queue ->
                        Hotseat (End athlete points (queue |> Queue.tick))

                    Play _ _ _ _ ->
                        game

            Single turn ->
                Debug.todo "Single mode not implemented."


skip : Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
skip seed words game =
    let
        ignore =
            ( game, seed, Nothing )

        check queue gameCreator =
            case Queue.pop queue of
                ( _, Nothing ) ->
                    nextStatus seed words game

                ( ann, Just newQueue ) ->
                    ( gameCreator newQueue
                    , seed
                    , Just (Announcement.toMessage ann)
                    )
    in
    case getQueue game of
        Just q ->
            if Queue.peek q |> Announcement.isSkippable then
                case game of
                    Hotseat turn ->
                        case turn of
                            GameStart queue ->
                                check queue (\newQueue -> Hotseat (GameStart newQueue))

                            RoundStart score athlete cnts queue ->
                                check queue (\newQueue -> Hotseat (RoundStart score athlete cnts newQueue))

                            PlayCorrect score athlete cnts queue ->
                                check queue (\newQueue -> Hotseat (PlayCorrect score athlete cnts newQueue))

                            PlayWrong score athlete cnts queue ->
                                check queue (\newQueue -> Hotseat (PlayWrong score athlete cnts newQueue))

                            RoundEnd score athlete queue ->
                                check queue (\newQueue -> Hotseat (RoundEnd score athlete newQueue))

                            Assessment score athlete queue ->
                                check queue (\newQueue -> Hotseat (Assessment score athlete newQueue))

                            End athlete points queue ->
                                check queue (\newQueue -> Hotseat (End athlete points newQueue))

                            Play _ _ _ _ ->
                                nextStatus seed words game

                    Single turn ->
                        Debug.todo "Single mode not implemented."

            else
                ignore

        Nothing ->
            nextStatus seed words game


userInput : String -> Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
userInput input seed words game =
    case game of
        Hotseat (Play score athlete previousInput cnts) ->
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
                        , seed = seed
                        }

                message =
                    case newGame of
                        Hotseat (PlayCorrect _ _ _ _) ->
                            Just (Message.CorrectAthleteInput athlete newInput)

                        Hotseat (PlayWrong _ _ _ _) ->
                            Just (Message.WrongAthleteInput athlete newInput)

                        Single _ ->
                            Debug.todo "Single mode not implemented."

                        _ ->
                            Nothing
            in
            ( newGame, newSeed, message )

        Hotseat _ ->
            ( game, seed, Nothing )

        Single _ ->
            Debug.todo "Single mode not implemented."



--------------- INTERNAL ---------------
--
-- GAME TURN GENERATION


startRound : { athlete : Athlete, score : PlayingScore, seed : Random.Seed } -> ( Game, Random.Seed )
startRound { athlete, score, seed } =
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
                score
                athlete
                (Constraints.serve initial)
                (Queue.singleton ann)
    in
    ( Hotseat newGame
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


athleteInputDone : { input : String, words : Words, constraints : Constraints, score : PlayingScore, athlete : Athlete, seed : Random.Seed } -> ( Game, Random.Seed, Maybe Message )
athleteInputDone { input, words, constraints, score, athlete, seed } =
    let
        playWrongWith messageFn =
            playWrong
                { messageFn = messageFn
                , score = score
                , athlete = athlete
                , constraints = constraints
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
        ( Hotseat (Play score athlete input constraints)
        , seed
        , Nothing
        )


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
            Hotseat (RoundEnd score winner (Queue.singleton (message |> Announcement.create)))
    in
    ( newGame, newSeed )


assessment : { score : PlayingScore, athlete : Athlete, seed : Random.Seed } -> ( Game, Random.Seed )
assessment { score, athlete, seed } =
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
            Hotseat (Assessment score athlete (Queue.fromList ann anns))
    in
    ( newGame, newSeed )



-- OTHER


getQueue : Game -> Maybe Queue
getQueue game =
    case game of
        Hotseat turn ->
            case turn of
                GameStart queue ->
                    Just queue

                RoundStart _ _ _ queue ->
                    Just queue

                PlayCorrect _ _ _ queue ->
                    Just queue

                PlayWrong _ _ _ queue ->
                    Just queue

                RoundEnd _ _ queue ->
                    Just queue

                Assessment _ _ queue ->
                    Just queue

                End _ _ queue ->
                    Just queue

                Play _ _ _ _ ->
                    Nothing

        Single turn ->
            Debug.todo "Single mode not implemented."


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
        Hotseat turn ->
            case turn of
                GameStart queue ->
                    check queue (\newQueue -> Hotseat (GameStart newQueue))

                RoundStart score athlete cnts queue ->
                    check queue (\newQueue -> Hotseat (RoundStart score athlete cnts newQueue))

                PlayCorrect score athlete cnts queue ->
                    check queue (\newQueue -> Hotseat (PlayCorrect score athlete cnts newQueue))

                PlayWrong score athlete cnts queue ->
                    check queue (\newQueue -> Hotseat (PlayWrong score athlete cnts newQueue))

                RoundEnd score athlete queue ->
                    check queue (\newQueue -> Hotseat (RoundEnd score athlete newQueue))

                Assessment score athlete queue ->
                    check queue (\newQueue -> Hotseat (Assessment score athlete newQueue))

                End athlete points queue ->
                    check queue (\newQueue -> Hotseat (End athlete points newQueue))

                Play _ _ _ _ ->
                    ( game, seed, Nothing )

        Single _ ->
            Debug.todo "Single mode not implemented yet."


nextStatus : Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
nextStatus seed words game =
    let
        addMessage ann ( g, s ) =
            ( g, s, Just (Announcement.toMessage ann) )
    in
    case game of
        Hotseat turn ->
            case turn of
                GameStart queue ->
                    startRound
                        { score = Score.emptyPlayingScore
                        , athlete = AthleteB
                        , seed = seed
                        }
                        |> addMessage (Queue.peek queue)

                RoundStart score athlete cnts queue ->
                    ( startPlay
                        { score = score
                        , athlete = athlete
                        , constraints = cnts
                        }
                    , seed
                    )
                        |> addMessage (Queue.peek queue)

                Play score athlete input cnts ->
                    athleteInputDone
                        { input = input
                        , words = words
                        , score = score
                        , athlete = athlete
                        , constraints = cnts
                        , seed = seed
                        }

                PlayCorrect score athlete cnts queue ->
                    ( startPlay
                        { score = score
                        , athlete = Utils.oppositeAthlete athlete
                        , constraints = cnts
                        }
                    , seed
                    )
                        |> addMessage (Queue.peek queue)

                PlayWrong score athlete _ queue ->
                    endRound
                        { winner = Utils.oppositeAthlete athlete
                        , score = score
                        , seed = seed
                        }
                        |> addMessage (Queue.peek queue)

                RoundEnd score athlete queue ->
                    case score of
                        PlayingScore playingScore ->
                            assessment
                                { score = playingScore
                                , athlete = athlete
                                , seed = seed
                                }
                                |> addMessage (Queue.peek queue)

                        WinnerScore winner loserScore ->
                            ( game, seed, Nothing )

                Assessment score athlete queue ->
                    startRound
                        { score = score
                        , athlete = Utils.oppositeAthlete athlete
                        , seed = seed
                        }
                        |> addMessage (Queue.peek queue)

                End winner loserPoints queue ->
                    ( game, seed, Nothing )

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
            Hotseat
                (PlayCorrect
                    score
                    athlete
                    newCnts
                    (Queue.singleton (message |> Announcement.createUnskippable))
                )
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
                    (Queue.singleton (message |> Announcement.createUnskippable))
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
