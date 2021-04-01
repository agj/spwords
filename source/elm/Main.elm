module Main exposing (..)

import Announcement exposing (Announcement)
import Athlete exposing (..)
import Browser
import Browser.Events
import Constraints exposing (Constraints)
import CustomEl
import Dict exposing (Dict)
import Doc
import Doc.EmuDecode
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


type Game
    = Hotseat Turn
    | Single Turn


type Turn
    = GameStart Announcement
    | Rules Announcement
    | TurnStart PlayingScore Athlete Constraints Announcement
    | Play PlayingScore Athlete String Constraints
    | PlayCorrect Score Athlete Constraints Announcement
    | PlayWrong Score Athlete Constraints Announcement
    | RoundEnd Score Announcement
    | NewRound PlayingScore Announcement
    | Tally Score Announcement
    | End Athlete Points Announcement



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

        Inputted text ->
            if isEnter text then
                ( pressedEnter model
                , Cmd.none
                )

            else
                case model.status of
                    Playing _ _ (Hotseat (Play _ _ _ _)) ->
                        ( athleteInput text model
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

                    TurnStart score athlete cnts ann ->
                        { model | status = Playing words passed (Hotseat (TurnStart score athlete cnts (ann |> Announcement.tick))) }

                    PlayCorrect score athlete cnts ann ->
                        { model | status = Playing words passed (Hotseat (PlayCorrect score athlete cnts (ann |> Announcement.tick))) }

                    PlayWrong score athlete cnts ann ->
                        { model | status = Playing words passed (Hotseat (PlayWrong score athlete cnts (ann |> Announcement.tick))) }

                    _ ->
                        model

            _ ->
                model


pressedEnter : Model -> Model
pressedEnter model =
    case model.status of
        Ready _ _ _ ->
            startGame model

        Playing _ _ game ->
            case game of
                Hotseat turn ->
                    case turn of
                        GameStart _ ->
                            showRules model

                        Rules _ ->
                            startTurn model

                        TurnStart _ _ _ _ ->
                            model

                        Play _ _ _ _ ->
                            checkInput model

                        PlayCorrect score _ _ _ ->
                            case score of
                                PlayingScore _ ->
                                    model

                                WinnerScore athlete loserScore ->
                                    model

                        PlayWrong score _ _ _ ->
                            case score of
                                PlayingScore _ ->
                                    startTurn model

                                WinnerScore athlete loserScore ->
                                    model

                        _ ->
                            model

                Single turn ->
                    Debug.todo "Single mode not implemented."

        _ ->
            model


checkAnnouncementDone : Model -> Model
checkAnnouncementDone model =
    let
        check stepper ann =
            if Announcement.isFinished ann then
                stepper model

            else
                model
    in
    case model.status of
        Ready _ _ _ ->
            model

        Playing _ _ (Hotseat turn) ->
            case turn of
                GameStart ann ->
                    check showRules ann

                Rules ann ->
                    check startTurn ann

                TurnStart _ _ _ ann ->
                    check startPlay ann

                PlayCorrect score _ _ ann ->
                    case score of
                        PlayingScore _ ->
                            check startPlay ann

                        WinnerScore _ _ ->
                            model

                PlayWrong score _ _ ann ->
                    case score of
                        PlayingScore _ ->
                            check startTurn ann

                        WinnerScore _ _ ->
                            model

                _ ->
                    model

        Playing _ _ (Single turn) ->
            Debug.todo "Single mode not implemented."

        _ ->
            model



-- STATUS ADVANCING


startGame : Model -> Model
startGame model =
    case model.status of
        Ready words passed ann ->
            let
                newPassed =
                    passed
                        |> Passed.push (Announcement.toMessage ann)

                game =
                    Hotseat
                        (GameStart
                            (Texts.gameStart
                                { athleteA = "left"
                                , athleteB = "right"
                                }
                                |> Announcement.create
                            )
                        )
            in
            { model | status = Playing words newPassed game }

        _ ->
            model


showRules : Model -> Model
showRules model =
    case model.status of
        Playing words passed (Hotseat (GameStart ann)) ->
            let
                newPassed =
                    passed
                        |> Passed.push (Announcement.toMessage ann)

                newGame =
                    Hotseat
                        (Rules
                            (Texts.rules
                                |> Announcement.create
                            )
                        )
            in
            { model | status = Playing words newPassed newGame }

        _ ->
            model


startTurn : Model -> Model
startTurn model =
    let
        doIt { athlete, passed, words, ann, score } =
            let
                ( initial, seed1 ) =
                    randomLetter model.randomSeed Texts.alphabet

                newPassed =
                    passed
                        |> Passed.push (Announcement.toMessage ann)

                ( turnAndLetter, newSeed ) =
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

                newGame =
                    Hotseat
                        (TurnStart
                            score
                            athlete
                            (Constraints.serve initial)
                            (Announcement.create turnAndLetter)
                        )
            in
            { model
                | status = Playing words newPassed newGame
                , randomSeed = newSeed
            }
    in
    case model.status of
        Playing words passed (Hotseat (Rules ann)) ->
            doIt
                { athlete = AthleteA
                , score = Score.emptyPlayingScore
                , passed = passed
                , words = words
                , ann = ann
                }

        Playing words passed (Hotseat (PlayWrong (PlayingScore score) athlete cnts ann)) ->
            doIt
                { athlete = oppositeAthlete athlete
                , score = score
                , passed = passed
                , words = words
                , ann = ann
                }

        _ ->
            model


startPlay : Model -> Model
startPlay model =
    case model.status of
        Playing words passed (Hotseat (TurnStart score athlete cnts ann)) ->
            let
                newPassed =
                    passed
                        |> Passed.push (Announcement.toMessage ann)
            in
            { model
                | status = Playing words newPassed (Hotseat (Play score athlete "" cnts))
            }

        Playing words passed (Hotseat (PlayCorrect (PlayingScore score) athlete cnts ann)) ->
            let
                newPassed =
                    passed
                        |> Passed.push (Announcement.toMessage ann)

                newAthlete =
                    oppositeAthlete athlete
            in
            { model
                | status = Playing words newPassed (Hotseat (Play score newAthlete "" cnts))
            }

        _ ->
            model


athleteInput : String -> Model -> Model
athleteInput text model =
    checkPartialInput <|
        case model.status of
            Playing words passed (Hotseat (Play score athlete oldText cnts)) ->
                { model | status = Playing words passed (Hotseat (Play score athlete (oldText ++ text) cnts)) }

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

                TurnStart _ _ _ ann ->
                    tickerEl (tickerAnnouncement ann) passed

                Play _ athlete text _ ->
                    tickerEl (tickerPlay athlete text) passed

                PlayCorrect _ athlete _ ann ->
                    tickerEl (tickerAnnouncement ann) passed

                PlayWrong _ athlete _ ann ->
                    tickerEl (tickerAnnouncement ann) passed

                _ ->
                    none

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


getActiveAthlete : Status -> Maybe Athlete
getActiveAthlete status =
    case status of
        Playing _ _ (Hotseat (Play _ athlete _ _)) ->
            Just athlete

        _ ->
            Nothing


oppositeAthlete : Athlete -> Athlete
oppositeAthlete athlete =
    case athlete of
        AthleteA ->
            AthleteB

        AthleteB ->
            AthleteA
