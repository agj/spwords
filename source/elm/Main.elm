module Main exposing (..)

import Athlete exposing (..)
import Browser
import Browser.Events
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
import Palette
import Random
import Return as R exposing (Return)
import Texts
import Ticker exposing (Ticker, inputCorrect)
import Ticker.Text as Text
import Ticker.Text.Constraints as Constraints exposing (Constraints)
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
    , game : GameStatus
    , viewport : Viewport
    , randomSeed : Random.Seed
    }


type GameStatus
    = GameLoading
    | GameIntro Words
    | GamePlaying Words
    | WordsLoadError Http.Error


type alias Flags =
    { viewport : Viewport
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { ticker =
            Ticker.empty
                |> Ticker.queueUp
                    (Text.QueuedInstruction (Texts.comments.loading |> emu Dict.empty))
      , game = GameLoading
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
            ( { model | ticker = Ticker.tick model.ticker }
            , Cmd.none
            )

        Inputted text ->
            case model.game of
                GameIntro _ ->
                    if isEnter text then
                        ( { model | ticker = Ticker.enter model.ticker }
                            |> startGame
                        , Cmd.none
                        )

                    else
                        modelCmd

                GamePlaying words ->
                    if isEnter text then
                        case Ticker.current model.ticker of
                            Just (Text.ActiveAthleteInput athlete _ _) ->
                                ( model |> checkInput words
                                , Cmd.none
                                )

                            Just _ ->
                                ( { model | ticker = Ticker.enter model.ticker }
                                , Cmd.none
                                )

                            Nothing ->
                                modelCmd

                    else
                        ( { model | ticker = Ticker.input text model.ticker }
                            |> checkPartialInput words
                        , Cmd.none
                        )

                GameLoading ->
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
            case model.game of
                GameLoading ->
                    case result of
                        Ok words ->
                            ( { model
                                | game = GameIntro (Words.parse words)
                                , ticker =
                                    model.ticker
                                        |> Ticker.queueUp
                                            (Text.QueuedAnnouncement (Texts.comments.toStart |> emu Dict.empty))
                              }
                            , Cmd.none
                            )

                        Err err ->
                            ( { model | game = WordsLoadError err }
                            , Cmd.none
                            )

                GameIntro _ ->
                    modelCmd

                GamePlaying _ ->
                    modelCmd

                WordsLoadError _ ->
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
    column [ height fill, width fill ]
        [ bar AthleteA (TimeLeft 0.5)
        , ticker model
        , bar AthleteB (TimeLeft 0.5)
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

        passedTexts =
            Ticker.passed model.ticker
                |> List.map tickerText

        tickerTexts =
            (case Ticker.current model.ticker of
                Just cur ->
                    tickerActive cur :: passedTexts

                Nothing ->
                    passedTexts
            )
                |> List.reverse
                |> List.intersperse (text " ")

        cursor =
            el
                [ width (px 5)
                , height (px (Palette.textSizeLarger * 2))
                , Background.color <|
                    case Ticker.current model.ticker of
                        Just (Text.ActiveAthleteInput athlete _ _) ->
                            Palette.athleteA

                        _ ->
                            Palette.transparent
                ]
                none
    in
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
                tickerTexts
            )
        , cursor
        ]


tickerActive : Text.Active -> Element Msg
tickerActive ta =
    case ta of
        Text.ActiveAnnouncement txt ticks ->
            fromDocParagraph (Doc.Util.paragraphLeft ticks txt)

        Text.ActiveInstruction txt ticks ->
            fromDocParagraph (Doc.Util.paragraphLeft ticks txt)

        Text.ActiveAthleteInput athlete txt _ ->
            el
                [ Font.color Palette.athleteA
                , Font.underline
                , Font.bold
                ]
                (text (String.toUpper txt))


tickerText : Text.Text -> Element Msg
tickerText tt =
    case tt of
        Text.InterruptedAnnouncement txt ticks ->
            fromDocParagraph (Doc.Util.paragraphAppend "—" (Doc.Util.paragraphLeft ticks txt))

        Text.FinishedAnnouncement txt ->
            fromDocParagraph txt

        Text.Instruction txt ->
            fromDocParagraph txt

        Text.CorrectAthleteInput athlete txt ->
            el
                [ Font.color Palette.athleteA
                , Font.bold
                ]
                (text (String.toUpper txt ++ "✔"))

        Text.WrongAthleteInput athlete txt ->
            el
                [ Font.color Palette.athleteA
                , Font.strike
                , Font.bold
                ]
                (text (String.toUpper txt))


type TimeLeft
    = TimeLeft Float


bar : Athlete -> TimeLeft -> Element Msg
bar athlete (TimeLeft timeLeft) =
    let
        filledPortion =
            round (timeLeft * 10000)

        emptyPortion =
            10000 - filledPortion

        filledColor =
            case athlete of
                AthleteA ->
                    Palette.athleteA

                AthleteB ->
                    Palette.athleteB

        emptyColor =
            case athlete of
                AthleteA ->
                    Palette.athleteADark

                AthleteB ->
                    Palette.athleteBDark
    in
    row
        [ width fill
        , height (px Palette.spaceSmall)
        ]
        [ el
            [ width (fillPortion filledPortion)
            , height fill
            , Background.color filledColor
            ]
            none
        , el
            [ width (fillPortion emptyPortion)
            , height fill
            , Background.color emptyColor
            ]
            none
        ]


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
            Just AthleteA ->
                [ Font.color Palette.athleteA ]

            Just AthleteB ->
                [ Font.color Palette.athleteB ]

            Nothing ->
                []
        ]



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
    case model.game of
        GameIntro words ->
            let
                vars =
                    Dict.fromList
                        [ ( "turn", "player" )
                        , ( "letter", initial |> String.fromChar )
                        , ( "athleteA", "player" )
                        , ( "athleteB", "computer" )
                        ]

                ( initial, seed1 ) =
                    randomLetter model.randomSeed Texts.alphabet

                start =
                    Texts.comments.start
                        |> emu vars

                rules =
                    Texts.comments.rules
                        |> emu vars

                ( turnAndLetter, newSeed ) =
                    Texts.comments.turnAndLetter
                        |> emuRandomString seed1 vars
            in
            { model
                | game = GamePlaying words
                , ticker =
                    model.ticker
                        |> Ticker.queueUp (Text.QueuedAnnouncement start)
                        |> Ticker.queueUp (Text.QueuedAnnouncement rules)
                        |> Ticker.queueUp (Text.QueuedInstruction turnAndLetter)
                        |> Ticker.queueUp (Text.QueuedAthleteInput AthleteA (Constraints.serve initial))
                , randomSeed = newSeed
            }

        _ ->
            model


checkPartialInput : Words -> Model -> Model
checkPartialInput words model =
    case Ticker.current model.ticker of
        Just (Text.ActiveAthleteInput athlete txt cnts) ->
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
        Just (Text.ActiveAthleteInput athlete txt cnts) ->
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
        Just (Text.ActiveAthleteInput athlete previousWord cnts) ->
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
                        |> emuRandomString model.randomSeed Dict.empty
            in
            { model
                | ticker =
                    Ticker.inputCorrect model.ticker
                        |> Ticker.queueUp (Text.QueuedInstruction message)
                        |> Ticker.queueUp (Text.QueuedAthleteInput AthleteA newCnts)
                , randomSeed = newSeed
            }

        _ ->
            model


inputWrong : List String -> Model -> Model
inputWrong messages model =
    case Ticker.current model.ticker of
        Just (Text.ActiveAthleteInput athlete previousWord cnts) ->
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

                vars =
                    List.append
                        [ ( "initial", Constraints.getInitial newCnts |> String.fromChar ) ]
                        (case Constraints.getIncorporates newCnts of
                            Just char ->
                                [ ( "incorporates", char |> String.fromChar ) ]

                            Nothing ->
                                []
                        )
                        |> Dict.fromList

                ( message, newSeed ) =
                    messages
                        |> emuRandomString model.randomSeed vars
            in
            { model
                | ticker =
                    Ticker.inputWrong model.ticker
                        |> Ticker.queueUp (Text.QueuedInstruction message)
                        |> Ticker.queueUp (Text.QueuedAthleteInput AthleteA newCnts)
                , randomSeed = newSeed
            }

        _ ->
            model


isEnter text =
    text == "\n"


emu : Dict String String -> String -> Paragraph.Paragraph
emu vars str =
    Doc.EmuDecode.fromEmu str
        |> Doc.content
        |> List.head
        |> Maybe.withDefault Paragraph.empty
        |> replaceVars vars


replaceVars : Dict String String -> Paragraph.Paragraph -> Paragraph.Paragraph
replaceVars vars par =
    let
        setAthleteStyle txt =
            case Doc.Text.content txt of
                "athleteA" ->
                    txt |> Doc.Text.mapFormat (Doc.Format.setAthlete (Just AthleteA))

                "athleteB" ->
                    txt |> Doc.Text.mapFormat (Doc.Format.setAthlete (Just AthleteB))

                _ ->
                    txt

        replaceVar txt =
            let
                content =
                    Doc.Text.content txt

                format =
                    Doc.Text.format txt
            in
            if Doc.Format.isVar format then
                txt
                    |> Doc.Text.mapFormat (Doc.Format.setVar False)
                    |> setAthleteStyle
                    |> Doc.Text.setContent
                        (Dict.get content vars |> Maybe.withDefault content)

            else
                txt
    in
    Paragraph.content par
        |> List.map replaceVar
        |> Paragraph.create


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


emuRandomString : Random.Seed -> Dict String String -> List String -> ( Paragraph, Random.Seed )
emuRandomString seed vars strings =
    randomString seed strings
        |> Tuple.mapFirst (emu vars)


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
