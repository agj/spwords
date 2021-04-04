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
import Doc.Paragraph exposing (Paragraph)
import Random
import Score exposing (PlayingScore, Points, Score(..))
import Texts
import Ticker.Active as Active exposing (Active)
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


getActive : Game -> Active
getActive game =
    case game of
        Hotseat turn ->
            case turn of
                GameStart ann ->
                    Active.fromAnnouncement ann

                Rules ann ->
                    Active.fromAnnouncement ann

                RoundStart _ _ _ ann ->
                    Active.fromAnnouncement ann

                PlayCorrect _ _ _ ann ->
                    Active.fromAnnouncement ann

                PlayWrong _ _ _ ann ->
                    Active.fromAnnouncement ann

                RoundEnd _ _ ann ->
                    Active.fromAnnouncement ann

                Tally _ _ ann ->
                    Active.fromAnnouncement ann

                Assessment _ _ ann ->
                    Active.fromAnnouncement ann

                NewRound _ _ ann ->
                    Active.fromAnnouncement ann

                End _ _ ann ->
                    Active.fromAnnouncement ann

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

                _ ->
                    nextStatus seed words game

        Single turn ->
            Debug.todo "Single mode not implemented."


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



-- OTHER


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


checkAnnouncementDone : Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
checkAnnouncementDone seed words game =
    case getAnnouncement game of
        Just ann ->
            if Announcement.isFinished ann then
                nextStatus seed words game

            else
                ( game, seed, Nothing )

        Nothing ->
            ( game, seed, Nothing )


nextStatus : Random.Seed -> Words -> Game -> ( Game, Random.Seed, Maybe Message )
nextStatus seed words game =
    let
        addMessage ann ( g, s ) =
            ( g, s, Just (Announcement.toMessage ann) )
    in
    case game of
        Hotseat turn ->
            case turn of
                GameStart ann ->
                    ( showRules, seed )
                        |> addMessage ann

                Rules ann ->
                    startRound
                        { score = Score.emptyPlayingScore
                        , athlete = AthleteB
                        , seed = seed
                        }
                        |> addMessage ann

                RoundStart score athlete cnts ann ->
                    ( startPlay
                        { score = score
                        , athlete = athlete
                        , constraints = cnts
                        }
                    , seed
                    )
                        |> addMessage ann

                Play score athlete input cnts ->
                    athleteInputDone
                        { input = input
                        , words = words
                        , score = score
                        , athlete = athlete
                        , constraints = cnts
                        , seed = seed
                        }

                PlayCorrect score athlete cnts ann ->
                    ( startPlay
                        { score = score
                        , athlete = Utils.oppositeAthlete athlete
                        , constraints = cnts
                        }
                    , seed
                    )
                        |> addMessage ann

                PlayWrong score athlete _ ann ->
                    endRound
                        { winner = Utils.oppositeAthlete athlete
                        , score = score
                        , seed = seed
                        }
                        |> addMessage ann

                RoundEnd score athlete ann ->
                    case score of
                        PlayingScore playingScore ->
                            tally
                                { score = playingScore
                                , athlete = athlete
                                , seed = seed
                                }
                                |> addMessage ann

                        WinnerScore winner loserScore ->
                            ( game, seed, Nothing )

                Tally score athlete ann ->
                    assessment
                        { score = score
                        , athlete = athlete
                        , seed = seed
                        }
                        |> addMessage ann

                Assessment score athlete ann ->
                    newRound
                        { score = score
                        , athlete = Utils.oppositeAthlete athlete
                        , seed = seed
                        }
                        |> addMessage ann

                NewRound score athlete ann ->
                    startRound
                        { score = score
                        , athlete = athlete
                        , seed = seed
                        }
                        |> addMessage ann

                End winner loserPoints ann ->
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
