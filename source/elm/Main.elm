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
import Element.Region exposing (announce)
import Html exposing (Html)
import Http
import Levers
import Message exposing (Message)
import Palette
import Passed exposing (Passed)
import Random
import Return as R exposing (Return)
import Score exposing (..)
import Texts
import Ticker exposing (Ticker, inputCorrect)
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
    { ticker : Ticker
    , status : Status
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
    | PlayCorrect PlayingScore Athlete Announcement
    | PlayWrong Score Athlete ReasonWrong Announcement
    | RoundEnd Score Announcement
    | NewRound PlayingScore Announcement
    | Tally Score Announcement
    | End Athlete Points Announcement


type ReasonWrong
    = InitialWrong
    | IncorporatesWrong
    | AlreadyPlayed
    | NotAWord



-- INIT


type alias Flags =
    { viewport : Viewport
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { ticker = Ticker.empty
      , status = Loading (Announcement.create (Texts.comments.loading |> emu identity Dict.empty))
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
        modelCmd =
            ( model, Cmd.none )
    in
    case msg of
        Ticked _ ->
            ( { model
                | ticker = Ticker.tick model.ticker
                , status = tickStatus model.status
              }
            , Cmd.none
            )

        Inputted text ->
            case model.status of
                Ready words passed ann ->
                    if isEnter text then
                        ( startGame model
                        , Cmd.none
                        )

                    else
                        modelCmd

                Playing words passed game ->
                    if isEnter text then
                        case game of
                            Hotseat turn ->
                                case turn of
                                    GameStart _ ->
                                        ( showRules model
                                        , Cmd.none
                                        )

                                    Rules _ ->
                                        ( startTurn model
                                        , Cmd.none
                                        )

                                    TurnStart _ _ _ _ ->
                                        ( startPlay model
                                        , Cmd.none
                                        )

                                    _ ->
                                        modelCmd

                            _ ->
                                modelCmd
                        -- case Ticker.current model.ticker of
                        --     Just (Message.ActiveAthleteInput athlete _ _) ->
                        --         ( model |> checkInput words
                        --         , Cmd.none
                        --         )
                        --     Just _ ->
                        --         ( { model | ticker = Ticker.enter model.ticker }
                        --         , Cmd.none
                        --         )
                        --     Nothing ->
                        --         modelCmd

                    else
                        case game of
                            Hotseat turn ->
                                case turn of
                                    Play _ _ _ _ ->
                                        ( userInput text model
                                        , Cmd.none
                                        )

                                    _ ->
                                        modelCmd

                            _ ->
                                modelCmd

                -- case Ticker.current model.ticker of
                --     Just (Message.ActiveAthleteInput AthleteA _ _) ->
                --         ( { model | ticker = Ticker.input text model.ticker }
                --             |> checkPartialInput words
                --         , Cmd.none
                --         )
                --     Just (Message.ActiveAthleteInput AthleteB _ _) ->
                --         -- For now, user inputs for computer too.
                --         ( { model | ticker = Ticker.input text model.ticker }
                --             |> checkPartialInput words
                --         , Cmd.none
                --         )
                --     _ ->
                --         modelCmd
                Loading _ ->
                    modelCmd

                WordsLoadError _ ->
                    modelCmd

        -- INITIALIZATION STAGE
        --
        GotSeed seed ->
            ( { model | randomSeed = seed }
            , Cmd.none
            )

        GotWords result ->
            case model.status of
                Loading ann ->
                    case result of
                        Ok words ->
                            ( { model
                                | status =
                                    Ready (Words.parse words)
                                        (Passed.empty |> Passed.push (Announcement.toMessage ann))
                                        (Announcement.create (Texts.comments.toStart |> emu identity Dict.empty))
                              }
                            , Cmd.none
                            )

                        Err err ->
                            ( { model | status = WordsLoadError err }
                            , Cmd.none
                            )

                _ ->
                    modelCmd

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
            modelCmd


tickStatus : Status -> Status
tickStatus status =
    case status of
        Loading ann ->
            Loading (Announcement.tick ann)

        Ready words passed ann ->
            Ready words passed (Announcement.tick ann)

        Playing words passed game ->
            case game of
                Hotseat turn ->
                    case turn of
                        GameStart ann ->
                            Playing words passed (Hotseat (GameStart (ann |> Announcement.tick)))

                        Rules ann ->
                            Playing words passed (Hotseat (Rules (ann |> Announcement.tick)))

                        TurnStart score athlete cnts ann ->
                            Playing words passed (Hotseat (TurnStart score athlete cnts (ann |> Announcement.tick)))

                        _ ->
                            status

                _ ->
                    status

        _ ->
            status



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
            case Ticker.current model.ticker of
                Just (Message.ActiveAthleteInput athlete _ _) ->
                    Just athlete

                _ ->
                    Nothing

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
                    case Ticker.current model.ticker of
                        Just (Message.ActiveAthleteInput athlete _ _) ->
                            athleteColor athlete

                        _ ->
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

        Playing _ passed game ->
            case game of
                Hotseat turn ->
                    case turn of
                        GameStart ann ->
                            tickerEl (tickerAnnouncement ann) passed

                        Rules ann ->
                            tickerEl (tickerAnnouncement ann) passed

                        TurnStart _ _ _ ann ->
                            tickerEl (tickerAnnouncement ann) passed

                        Play _ athlete text _ ->
                            tickerEl (tickerPlay athlete text) passed

                        _ ->
                            none

                _ ->
                    none

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


tickerActive : Message.Active -> Element Msg
tickerActive ta =
    case ta of
        Message.ActiveAnnouncement txt ticks ->
            fromDocParagraph (Doc.Util.paragraphLeft ticks txt)

        Message.ActiveInstruction txt ticks ->
            fromDocParagraph (Doc.Util.paragraphLeft ticks txt)

        Message.ActiveAthleteInput athlete txt _ ->
            el
                [ Font.color (athleteColor athlete)
                , Font.underline
                , Font.bold
                ]
                (text (String.toUpper txt))


tickerText : Message -> Element Msg
tickerText tt =
    case tt of
        Message.InterruptedAnnouncement txt ticks ->
            fromDocParagraph (Doc.Util.paragraphAppend "—" (Doc.Util.paragraphLeft ticks txt))

        Message.FinishedAnnouncement txt ->
            fromDocParagraph txt

        Message.Instruction txt ->
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


startGame : Model -> Model
startGame model =
    case model.status of
        Ready words passed ann ->
            let
                vars =
                    Dict.fromList
                        [ ( "athleteA", "left" )
                        , ( "athleteB", "right" )
                        ]

                setStyles txt =
                    case Doc.Text.content txt of
                        "athleteA" ->
                            txt |> Doc.Text.mapFormat (Doc.Format.setAthlete (Just AthleteA))

                        "athleteB" ->
                            txt |> Doc.Text.mapFormat (Doc.Format.setAthlete (Just AthleteB))

                        _ ->
                            txt

                newPassed =
                    passed
                        |> Passed.push (Announcement.toMessage ann)

                game =
                    Hotseat
                        (GameStart
                            (Texts.comments.start
                                |> emu setStyles vars
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
                            (Texts.comments.rules
                                |> emu identity Dict.empty
                                |> Announcement.create
                            )
                        )
            in
            { model | status = Playing words newPassed newGame }

        _ ->
            model


startTurn : Model -> Model
startTurn model =
    case model.status of
        Playing words passed (Hotseat (Rules ann)) ->
            let
                ( initial, seed1 ) =
                    randomLetter model.randomSeed Texts.alphabet

                vars =
                    Dict.fromList
                        [ ( "turn", "left" )
                        , ( "letter", initial |> String.fromChar )
                        ]

                setStyles txt =
                    case Doc.Text.content txt of
                        "turn" ->
                            txt |> Doc.Text.mapFormat (Doc.Format.setAthlete (Just AthleteA))

                        "letter" ->
                            txt |> Doc.Text.mapFormat (Doc.Format.setBold True)

                        _ ->
                            txt

                ( turnAndLetter, newSeed ) =
                    Texts.comments.turnAndLetter
                        |> emuRandomString seed1 setStyles vars

                newPassed =
                    passed
                        |> Passed.push (Announcement.toMessage ann)

                newGame =
                    Hotseat
                        (TurnStart
                            Score.emptyPlayingScore
                            AthleteB
                            (Constraints.serve initial)
                            (Announcement.create turnAndLetter)
                        )
            in
            { model
                | status = Playing words newPassed newGame
                , randomSeed = newSeed
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

        _ ->
            model


userInput : String -> Model -> Model
userInput text model =
    case model.status of
        Playing words passed (Hotseat (Play score athlete oldText cnts)) ->
            { model | status = Playing words passed (Hotseat (Play score athlete (oldText ++ text) cnts)) }

        _ ->
            model


checkPartialInput : Words -> Model -> Model
checkPartialInput words model =
    case Ticker.current model.ticker of
        Just (Message.ActiveAthleteInput athlete txt cnts) ->
            case Constraints.checkCandidate txt cnts words of
                Constraints.CandidateCorrect ->
                    model

                Constraints.CandidateInitialWrong ->
                    inputWrong Texts.comments.mistake.initial model

                Constraints.CandidateNotAWord ->
                    inputWrong Texts.comments.mistake.notAWord model

        Just _ ->
            model

        Nothing ->
            model


checkInput : Words -> Model -> Model
checkInput words model =
    case Ticker.current model.ticker of
        Just (Message.ActiveAthleteInput athlete txt cnts) ->
            case Constraints.check txt cnts words of
                Constraints.InputCorrect ->
                    inputCorrect model

                Constraints.InputInitialWrong ->
                    inputWrong Texts.comments.mistake.initial model

                Constraints.InputIncorporatesWrong ->
                    inputWrong Texts.comments.mistake.incorporates model

                Constraints.InputAlreadyPlayed ->
                    inputWrong Texts.comments.mistake.alreadyPlayed model

                Constraints.InputNotAWord ->
                    inputWrong Texts.comments.mistake.notAWord model

        _ ->
            model


inputCorrect : Model -> Model
inputCorrect model =
    case Ticker.current model.ticker of
        Just (Message.ActiveAthleteInput athlete previousWord cnts) ->
            let
                newCnts =
                    Constraints.rally
                        { initial = Constraints.getInitial cnts
                        , incorporates = Utils.stringLast previousWord |> Maybe.withDefault '?'
                        , played =
                            Constraints.getPlayed cnts
                                |> (::) previousWord
                        }

                ( message, newSeed ) =
                    Texts.comments.interjection
                        |> emuRandomString model.randomSeed identity Dict.empty

                newAthlete =
                    case athlete of
                        AthleteA ->
                            AthleteB

                        AthleteB ->
                            AthleteA
            in
            { model
                | ticker =
                    Ticker.inputCorrect model.ticker
                        |> Ticker.queueUp (Message.QueuedInstruction message)
                        |> Ticker.queueUp (Message.QueuedAthleteInput newAthlete newCnts)
                , randomSeed = newSeed
            }

        _ ->
            model


inputWrong : List String -> Model -> Model
inputWrong messages model =
    case Ticker.current model.ticker of
        Just (Message.ActiveAthleteInput athlete previousWord cnts) ->
            let
                newCnts =
                    case Constraints.getIncorporates cnts of
                        Just incorporates ->
                            Constraints.rally
                                { initial = Constraints.getInitial cnts
                                , incorporates = incorporates
                                , played = Constraints.getPlayed cnts
                                }

                        Nothing ->
                            Constraints.serve (Constraints.getInitial cnts)

                newAthlete =
                    case athlete of
                        AthleteA ->
                            AthleteB

                        AthleteB ->
                            AthleteA

                vars =
                    Dict.singleton "initial" (Constraints.getInitial newCnts |> String.fromChar)
                        |> (case Constraints.getIncorporates newCnts of
                                Just char ->
                                    Dict.insert "incorporates" (char |> String.fromChar)

                                Nothing ->
                                    identity
                           )

                setStyles txt =
                    let
                        boldify =
                            Doc.Text.mapFormat (Doc.Format.setBold True)
                    in
                    case Doc.Text.content txt of
                        "initial" ->
                            boldify txt

                        "incorporates" ->
                            boldify txt

                        _ ->
                            txt

                ( message, newSeed ) =
                    messages
                        |> emuRandomString model.randomSeed setStyles vars
            in
            { model
                | ticker =
                    Ticker.inputWrong model.ticker
                        |> Ticker.queueUp (Message.QueuedInstruction message)
                        |> Ticker.queueUp (Message.QueuedAthleteInput newAthlete newCnts)
                , randomSeed = newSeed
            }

        _ ->
            model


isEnter text =
    text == "\n"


emu : (Doc.Text.Text -> Doc.Text.Text) -> Dict String String -> String -> Paragraph.Paragraph
emu formatter vars str =
    Doc.EmuDecode.fromEmu str
        |> Doc.content
        |> List.head
        |> Maybe.withDefault Paragraph.empty
        |> replaceVars formatter vars


replaceVars : (Doc.Text.Text -> Doc.Text.Text) -> Dict String String -> Paragraph.Paragraph -> Paragraph.Paragraph
replaceVars formatter vars par =
    let
        replaceVar txt =
            let
                content =
                    Doc.Text.content txt
            in
            if txt |> Doc.Text.format |> Doc.Format.isVar then
                txt
                    |> Doc.Text.mapFormat (Doc.Format.setVar False)
                    |> formatter
                    |> Doc.Text.setContent
                        (Dict.get content vars |> Maybe.withDefault content)

            else
                txt
    in
    par
        |> Paragraph.mapContent (List.map replaceVar)


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


emuRandomString : Random.Seed -> (Doc.Text.Text -> Doc.Text.Text) -> Dict String String -> List String -> ( Paragraph, Random.Seed )
emuRandomString seed formatter vars strings =
    randomString seed strings
        |> Tuple.mapFirst (emu formatter vars)


randomString : Random.Seed -> List String -> ( String, Random.Seed )
randomString seed strings =
    randomItem seed strings
        |> Tuple.mapFirst (Maybe.withDefault "")


randomItem : Random.Seed -> List a -> ( Maybe a, Random.Seed )
randomItem seed list =
    Random.step (itemGenerator list) seed


itemGenerator : List a -> Random.Generator (Maybe a)
itemGenerator list =
    Random.int 0 (List.length list - 1)
        |> Random.map (indexToItem list)


indexToItem : List a -> Int -> Maybe a
indexToItem list index =
    list
        |> List.drop index
        |> List.head
