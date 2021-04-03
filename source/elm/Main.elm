module Main exposing (..)

import Announcement exposing (Announcement)
import Athlete exposing (..)
import Browser
import Browser.Events
import Constraints exposing (Constraints)
import CustomEl
import Dict exposing (Dict)
import Doc
import Doc.Format
import Doc.Paragraph as Paragraph exposing (Paragraph)
import Doc.Text
import Doc.Util
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Game exposing (Game(..), Turn(..))
import Html exposing (Html)
import Http
import Levers
import Message exposing (Message)
import Palette
import Passed exposing (Passed)
import Random
import Score exposing (..)
import Texts
import Time
import Utils exposing (..)
import Viewport exposing (Viewport)
import Words exposing (Words)



-- MAIN


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { status : Status
    , viewport : Viewport
    , randomSeed : Random.Seed
    }


type Status
    = Loading Announcement
    | Ready Words Passed Announcement
    | Playing Words Passed Game
    | WordsLoadError Http.Error



-- INIT


type alias Flags =
    { viewport : Viewport
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { status = Loading (Announcement.create Texts.loading)
      , viewport = flags.viewport
      , randomSeed = Random.initialSeed 64
      }
    , Cmd.batch
        [ Http.get
            { url = "data/words-en.txt"
            , expect = Http.expectString GotWords
            }
        , Random.generate GotSeed Random.independentSeed
        ]
    )



-- UPDATE


type Msg
    = Ticked Time.Posix
    | Inputted String
    | GotWords (Result Http.Error String)
    | Resized
    | GotViewport Viewport
    | GotSeed Random.Seed
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        default =
            ( model, Cmd.none )
    in
    case msg of
        Ticked _ ->
            ( tickStatus model
            , Cmd.none
            )

        Inputted input ->
            if isEnter input then
                ( pressedEnter model
                , Cmd.none
                )

            else
                case model.status of
                    Playing _ _ (Hotseat (Play score athlete previousInput cnts)) ->
                        ( athleteInput input previousInput score athlete cnts model
                        , Cmd.none
                        )

                    Playing _ _ (Single (Play _ _ _ _)) ->
                        Debug.todo "Single mode not implemented."

                    _ ->
                        default

        -- INITIALIZATION STAGE
        --
        GotSeed seed ->
            ( { model | randomSeed = seed }
            , Cmd.none
            )

        GotWords result ->
            ( gotWords result model
            , Cmd.none
            )

        -- OTHERS
        --
        Resized ->
            ( model
            , Viewport.get
            )

        GotViewport viewport ->
            ( { model | viewport = viewport }
            , Cmd.none
            )

        NoOp ->
            default


gotWords : Result Http.Error String -> Model -> Model
gotWords result model =
    case model.status of
        Loading ann ->
            case result of
                Ok words ->
                    { model
                        | status =
                            Ready (Words.parse words)
                                (Passed.empty |> Passed.push (Announcement.toMessage ann))
                                (Announcement.create Texts.ready)
                    }

                Err err ->
                    { model | status = WordsLoadError err }

        _ ->
            model


tickStatus : Model -> Model
tickStatus model =
    checkAnnouncementDone <|
        case model.status of
            Loading ann ->
                { model | status = Loading (Announcement.tick ann) }

            Ready words passed ann ->
                { model | status = Ready words passed (Announcement.tick ann) }

            Playing words passed (Hotseat turn) ->
                case turn of
                    GameStart ann ->
                        { model | status = Playing words passed (Hotseat (GameStart (ann |> Announcement.tick))) }

                    Rules ann ->
                        { model | status = Playing words passed (Hotseat (Rules (ann |> Announcement.tick))) }

                    RoundStart score athlete cnts ann ->
                        { model | status = Playing words passed (Hotseat (RoundStart score athlete cnts (ann |> Announcement.tick))) }

                    PlayCorrect score athlete cnts ann ->
                        { model | status = Playing words passed (Hotseat (PlayCorrect score athlete cnts (ann |> Announcement.tick))) }

                    PlayWrong score athlete cnts ann ->
                        { model | status = Playing words passed (Hotseat (PlayWrong score athlete cnts (ann |> Announcement.tick))) }

                    RoundEnd score athlete ann ->
                        { model | status = Playing words passed (Hotseat (RoundEnd score athlete (ann |> Announcement.tick))) }

                    Tally score athlete ann ->
                        { model | status = Playing words passed (Hotseat (Tally score athlete (ann |> Announcement.tick))) }

                    Assessment score athlete ann ->
                        { model | status = Playing words passed (Hotseat (Assessment score athlete (ann |> Announcement.tick))) }

                    NewRound score athlete ann ->
                        { model | status = Playing words passed (Hotseat (NewRound score athlete (ann |> Announcement.tick))) }

                    End athlete points ann ->
                        { model | status = Playing words passed (Hotseat (End athlete points (ann |> Announcement.tick))) }

                    Play _ _ _ _ ->
                        model

            Playing words passed (Single turn) ->
                Debug.todo "Single mode not implemented."

            _ ->
                model


checkAnnouncementDone : Model -> Model
checkAnnouncementDone model =
    case model.status of
        Ready _ _ _ ->
            model

        Playing _ _ game ->
            case Game.getAnnouncement game of
                Just ann ->
                    if Announcement.isFinished ann then
                        nextStatus model

                    else
                        model

                Nothing ->
                    model

        _ ->
            model


pressedEnter : Model -> Model
pressedEnter model =
    case model.status of
        Ready _ _ _ ->
            nextStatus model

        Playing _ _ (Hotseat turn) ->
            case turn of
                RoundStart _ _ _ _ ->
                    model

                PlayCorrect score _ _ _ ->
                    case score of
                        PlayingScore _ ->
                            model

                        WinnerScore _ _ ->
                            model

                PlayWrong _ _ _ _ ->
                    model

                _ ->
                    nextStatus model

        _ ->
            model


nextStatus : Model -> Model
nextStatus model =
    case model.status of
        Ready words passed ann ->
            { model | status = Playing words (Passed.pushAnnouncement ann passed) Game.startGame }

        Playing words passed (Hotseat turn) ->
            case turn of
                GameStart ann ->
                    { model | status = Playing words (Passed.pushAnnouncement ann passed) Game.showRules }

                Rules ann ->
                    startRound Score.emptyPlayingScore AthleteA ann model

                RoundStart score athlete cnts ann ->
                    startPlay score athlete cnts ann model

                Play _ _ _ _ ->
                    checkInput model

                PlayCorrect score athlete cnts ann ->
                    case score of
                        PlayingScore playingScore ->
                            startPlay playingScore (oppositeAthlete athlete) cnts ann model

                        WinnerScore winner loserScore ->
                            model

                PlayWrong score athlete _ ann ->
                    endRound score athlete ann model

                RoundEnd score athlete ann ->
                    case score of
                        PlayingScore playingScore ->
                            tally playingScore athlete ann model

                        WinnerScore winner loserScore ->
                            model

                Tally score athlete ann ->
                    assessment score athlete ann model

                Assessment score athlete ann ->
                    newRound score athlete ann model

                NewRound score athlete ann ->
                    startRound score athlete ann model

                End winner loserPoints ann ->
                    model

        Playing words passed (Single turn) ->
            Debug.todo "Single mode not implemented."

        _ ->
            model



-- STATUS GENERATION


startRound : PlayingScore -> Athlete -> Announcement -> Model -> Model
startRound score athlete ann model =
    case model.status of
        Playing words passed _ ->
            let
                ( newGame, newSeed ) =
                    Game.startRound
                        { score = score
                        , athlete = oppositeAthlete athlete
                        , seed = model.randomSeed
                        }
            in
            { model
                | status = Playing words (Passed.pushAnnouncement ann passed) newGame
                , randomSeed = newSeed
            }

        _ ->
            model


startPlay : PlayingScore -> Athlete -> Constraints -> Announcement -> Model -> Model
startPlay score athlete cnts ann model =
    case model.status of
        Playing words passed _ ->
            let
                newGame =
                    Game.startPlay
                        { score = score
                        , athlete = athlete
                        , constraints = cnts
                        }
            in
            { model | status = Playing words (Passed.pushAnnouncement ann passed) newGame }

        _ ->
            model


athleteInput : String -> String -> PlayingScore -> Athlete -> Constraints -> Model -> Model
athleteInput input previousInput score athlete cnts model =
    checkPartialInput <|
        case model.status of
            Playing words passed _ ->
                let
                    newGame =
                        Game.athleteInput
                            { input = input
                            , previousInput = previousInput
                            , score = score
                            , athlete = athlete
                            , constraints = cnts
                            }
                in
                { model | status = Playing words passed newGame }

            _ ->
                model


endRound : Score -> Athlete -> Announcement -> Model -> Model
endRound score athlete ann model =
    case model.status of
        Playing words passed _ ->
            let
                newPassed =
                    passed
                        |> Passed.pushAnnouncement ann

                ( newGame, newSeed ) =
                    Game.endRound
                        { winner = oppositeAthlete athlete
                        , score = score
                        , seed = model.randomSeed
                        }
            in
            { model
                | status = Playing words newPassed newGame
                , randomSeed = newSeed
            }

        _ ->
            model


tally : PlayingScore -> Athlete -> Announcement -> Model -> Model
tally score athlete ann model =
    scoreAthleteStatus
        Game.tally
        score
        athlete
        ann
        model


assessment : PlayingScore -> Athlete -> Announcement -> Model -> Model
assessment score athlete ann model =
    scoreAthleteStatus
        Game.assessment
        score
        athlete
        ann
        model


newRound : PlayingScore -> Athlete -> Announcement -> Model -> Model
newRound score athlete ann model =
    scoreAthleteStatus
        Game.newRound
        score
        (oppositeAthlete athlete)
        ann
        model


scoreAthleteStatus : ({ score : PlayingScore, athlete : Athlete, seed : Random.Seed } -> ( Game, Random.Seed )) -> PlayingScore -> Athlete -> Announcement -> Model -> Model
scoreAthleteStatus generator score athlete ann model =
    case model.status of
        Playing words passed _ ->
            let
                newPassed =
                    passed
                        |> Passed.pushAnnouncement ann

                ( newGame, newSeed ) =
                    generator
                        { athlete = athlete
                        , score = score
                        , seed = model.randomSeed
                        }
            in
            { model
                | status = Playing words newPassed newGame
                , randomSeed = newSeed
            }

        _ ->
            model



-- INPUT CHECKING


checkPartialInput : Model -> Model
checkPartialInput model =
    case model.status of
        Playing words _ (Hotseat (Play _ _ txt cnts)) ->
            case Constraints.checkCandidate txt cnts words of
                Constraints.CandidateCorrect ->
                    model

                Constraints.CandidateInitialWrong ->
                    inputWrong Texts.initialWrong model

                Constraints.CandidateNotAWord ->
                    inputWrong Texts.notAWord model

        Playing words passed (Single (Play score athlete txt cnts)) ->
            Debug.todo "Single mode not implemented."

        _ ->
            model


checkInput : Model -> Model
checkInput model =
    case model.status of
        Playing words _ (Hotseat (Play _ _ txt cnts)) ->
            case Constraints.check txt cnts words of
                Constraints.InputCorrect ->
                    inputCorrect model

                Constraints.InputInitialWrong ->
                    inputWrong Texts.initialWrong model

                Constraints.InputIncorporatesWrong ->
                    inputWrong Texts.incorporatesWrong model

                Constraints.InputAlreadyPlayed ->
                    inputWrong Texts.alreadyPlayed model

                Constraints.InputNotAWord ->
                    inputWrong Texts.notAWord model

        Playing words passed (Single (Play score athlete txt cnts)) ->
            Debug.todo "Single mode not implemented."

        _ ->
            model


inputCorrect : Model -> Model
inputCorrect model =
    case model.status of
        Playing words passed (Hotseat (Play score athlete newWord cnts)) ->
            let
                newCnts =
                    Constraints.rally
                        { initial = Constraints.getInitial cnts
                        , incorporates = Utils.stringLast newWord |> Maybe.withDefault '?'
                        , played =
                            Constraints.getPlayed cnts
                                |> (::) newWord
                        }

                newPassed =
                    passed
                        |> Passed.push (Message.CorrectAthleteInput athlete newWord)

                ( message, newSeed ) =
                    Texts.interjection model.randomSeed

                newGame =
                    Hotseat (PlayCorrect (PlayingScore score) athlete newCnts (message |> Announcement.create))
            in
            { model
                | status = Playing words newPassed newGame
                , randomSeed = newSeed
            }

        Playing words passed (Single (Play score athlete txt cnts)) ->
            Debug.todo "Single mode not implemented."

        _ ->
            model


inputWrong : (Texts.MistakeArguments -> ( Paragraph, Random.Seed )) -> Model -> Model
inputWrong messageFn model =
    case model.status of
        Playing words passed (Hotseat (Play score athlete txt cnts)) ->
            let
                newScore =
                    Score.increaseScore (oppositeAthlete athlete) score

                newPassed =
                    passed
                        |> Passed.push (Message.WrongAthleteInput athlete txt)

                ( message, newSeed ) =
                    messageFn
                        { initial = cnts |> Constraints.getInitial
                        , incorporates = cnts |> Constraints.getIncorporates
                        , seed = model.randomSeed
                        }

                newGame =
                    Hotseat
                        (PlayWrong
                            newScore
                            athlete
                            cnts
                            (message |> Announcement.create)
                        )
            in
            { model
                | status = Playing words newPassed newGame
                , randomSeed = newSeed
            }

        Playing words passed (Single (Play score athlete txt cnts)) ->
            Debug.todo "Single mode not implemented."

        _ ->
            model



-- VIEW


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> Document Msg
view model =
    { title = Texts.title
    , body =
        [ layout
            [ Font.family Palette.font
            , Font.size Palette.textSizeNormal
            , Font.color Palette.light
            , Background.color Palette.dark
            , padding 0
            ]
            (mainScreen model)
        ]
    }


mainScreen : Model -> Element Msg
mainScreen model =
    let
        activeAthlete =
            getActiveAthlete model.status

        isAthlete athlete =
            case activeAthlete of
                Just ath ->
                    athlete == ath

                _ ->
                    False
    in
    column [ height fill, width fill ]
        [ bar AthleteA (TimeLeft 0.5) (isAthlete AthleteA)
        , ticker model
        , bar AthleteB (TimeLeft 0.5) (isAthlete AthleteB)
        ]


ticker : Model -> Element Msg
ticker model =
    let
        input =
            Input.multiline
                [ width fill
                , height (px Palette.textSizeLarger)
                , Font.size Palette.textSizeSmall
                , Background.color Palette.transparent
                , Border.color Palette.transparent
                , focused [ Border.glow Palette.transparent 0 ]
                ]
                { text = ""
                , onChange = Inputted
                , placeholder = Nothing
                , label = Input.labelHidden ""
                , spellcheck = False
                }

        cursor =
            el
                [ width (px 5)
                , height (px (Palette.textSizeLarger * 2))
                , Background.color <|
                    case getActiveAthlete model.status of
                        Just athlete ->
                            athleteColor athlete

                        Nothing ->
                            Palette.transparent
                ]
                none

        toTickerTexts act passed =
            (act
                :: tickerPassed passed
            )
                |> List.reverse
                |> List.intersperse (text " ")

        tickerEl act passed =
            row
                [ centerY
                , width fill
                ]
                [ el
                    [ clip
                    , width fill
                    , height (px Palette.textSizeLarger)
                    , Font.size Palette.textSizeLarger
                    , inFront input
                    ]
                    (row
                        [ alignRight ]
                        (toTickerTexts act passed)
                    )
                , cursor
                ]
    in
    case model.status of
        Loading ann ->
            tickerEl (tickerAnnouncement ann) Passed.empty

        Ready _ passed ann ->
            tickerEl (tickerAnnouncement ann) passed

        Playing _ passed (Hotseat turn) ->
            case turn of
                GameStart ann ->
                    tickerEl (tickerAnnouncement ann) passed

                Rules ann ->
                    tickerEl (tickerAnnouncement ann) passed

                RoundStart _ _ _ ann ->
                    tickerEl (tickerAnnouncement ann) passed

                Play _ athlete text _ ->
                    tickerEl (tickerPlay athlete text) passed

                PlayCorrect _ _ _ ann ->
                    tickerEl (tickerAnnouncement ann) passed

                PlayWrong _ _ _ ann ->
                    tickerEl (tickerAnnouncement ann) passed

                RoundEnd _ _ ann ->
                    tickerEl (tickerAnnouncement ann) passed

                Tally _ _ ann ->
                    tickerEl (tickerAnnouncement ann) passed

                Assessment _ _ ann ->
                    tickerEl (tickerAnnouncement ann) passed

                NewRound _ _ ann ->
                    tickerEl (tickerAnnouncement ann) passed

                End _ _ ann ->
                    tickerEl (tickerAnnouncement ann) passed

        Playing _ passed (Single turn) ->
            Debug.todo "Single mode not implemented."

        WordsLoadError err ->
            none


tickerAnnouncement : Announcement -> Element Msg
tickerAnnouncement ann =
    Announcement.getCurrent ann
        |> fromDocParagraph


tickerPlay : Athlete -> String -> Element Msg
tickerPlay athlete txt =
    el
        [ Font.color (athleteColor athlete)
        , Font.underline
        , Font.bold
        ]
        (text (String.toUpper txt))


tickerPassed : Passed -> List (Element Msg)
tickerPassed passed =
    Passed.toList passed
        |> List.map tickerText


tickerText : Message -> Element Msg
tickerText tt =
    case tt of
        Message.InterruptedAnnouncement txt ticks ->
            fromDocParagraph (Doc.Util.paragraphAppend "—" (Doc.Util.paragraphLeft ticks txt))

        Message.FinishedAnnouncement txt ->
            fromDocParagraph txt

        Message.CorrectAthleteInput athlete txt ->
            el
                [ Font.color (athleteColor athlete)
                , Font.bold
                ]
                (text (String.toUpper txt ++ "✔"))

        Message.WrongAthleteInput athlete txt ->
            el
                [ Font.color (athleteColor athlete)
                , Font.strike
                , Font.bold
                , CustomEl.style "text-decoration-color" (Utils.toCssColor Palette.light)
                ]
                (text (String.toUpper txt))


type TimeLeft
    = TimeLeft Float


bar : Athlete -> TimeLeft -> Bool -> Element Msg
bar athlete (TimeLeft timeLeft) active =
    let
        filledPortion =
            round (timeLeft * 10000)

        emptyPortion =
            10000 - filledPortion

        fullHeight =
            Palette.spaceSmall * 3
    in
    el
        [ width fill
        , height (px fullHeight)
        ]
        (row
            [ case athlete of
                AthleteA ->
                    alignTop

                AthleteB ->
                    alignBottom
            , width fill
            , height
                (if active then
                    px fullHeight

                 else
                    px Palette.spaceSmall
                )
            ]
            [ el
                [ width (fillPortion emptyPortion)
                , height fill
                , Background.color (athleteColorDark athlete)
                ]
                none
            , el
                [ width (fillPortion filledPortion)
                , height fill
                , Background.color (athleteColor athlete)
                ]
                none
            ]
        )


fromDocParagraph : Paragraph.Paragraph -> Element msg
fromDocParagraph par =
    row [] <|
        List.map fromDocText (Paragraph.content par)


fromDocText : Doc.Text.Text -> Element msg
fromDocText txt =
    let
        textContent =
            Doc.Text.content txt
                |> String.toUpper

        style =
            Doc.Text.format txt
                |> getStyle
    in
    el style (text textContent)


getStyle : Doc.Format.Format -> List (Element.Attribute msg)
getStyle format =
    List.concat
        [ if Doc.Format.isBold format then
            [ Font.bold ]

          else
            []
        , if Doc.Format.isItalic format then
            [ Font.italic ]

          else
            []
        , case Doc.Format.athlete format of
            Just athlete ->
                [ Font.color (athleteColor athlete) ]

            Nothing ->
                []
        ]


athleteColor : Athlete -> Color
athleteColor athlete =
    case athlete of
        AthleteA ->
            Palette.athleteA

        AthleteB ->
            Palette.athleteB


athleteColorDark : Athlete -> Color
athleteColorDark athlete =
    case athlete of
        AthleteA ->
            Palette.athleteADark

        AthleteB ->
            Palette.athleteBDark



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize <|
            \w h -> Resized
        , Viewport.got GotViewport NoOp
        , Time.every Levers.tickInterval Ticked
        ]



-- OTHER


isEnter text =
    text == "\n"


getActiveAthlete : Status -> Maybe Athlete
getActiveAthlete status =
    case status of
        Playing _ _ game ->
            Game.getActiveAthlete game

        _ ->
            Nothing
