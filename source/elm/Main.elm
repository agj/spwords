module Main exposing (..)

import Browser
import Browser.Events
import Doc.Format
import Doc.Paragraph as Paragraph
import Doc.Text
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode as Decode
import Palette
import Process
import Random
import Return as R exposing (Return)
import Task
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
    }


type GameStatus
    = GameLoading
    | GameIntro Words
    | GamePlaying Words GameState
    | WordsLoadError Http.Error


type GameState
    = JustStarted
    | Serving { initial : Char }
    | Rallying { initial : Char, incorporates : Char, alreadyPlayed : List String }


type alias Flags =
    { viewport : Viewport
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { ticker =
            Ticker.empty |> Ticker.queueUp (Text.QueuedInstruction (paragraphFromString "(Loading…)"))
      , game = GameLoading
      , viewport = flags.viewport
      }
    , Http.get
        { url = "data/words-en.txt"
        , expect = Http.expectString GotWords
        }
    )



-- UPDATE


type Msg
    = Ticked Time.Posix
    | Inputted String
    | GotWords (Result Http.Error String)
    | RandomLetter Char
    | Resized
    | GotViewport Viewport
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        modelCmd =
            ( model, Cmd.none )
    in
    case ( msg, model.game ) of
        ( GotWords result, GameLoading ) ->
            case result of
                Ok words ->
                    ( { model
                        | game = GameIntro (Words.parse words)
                        , ticker =
                            model.ticker
                                |> Ticker.queueUp (Text.QueuedAnnouncement (paragraphFromString "(Done. Press Enter.)"))
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | game = WordsLoadError err }
                    , Cmd.none
                    )

        ( GotWords _, _ ) ->
            modelCmd

        ( Ticked _, _ ) ->
            ( { model | ticker = Ticker.tick model.ticker }
            , Cmd.none
            )

        ( Inputted text, GameIntro _ ) ->
            if isEnter text then
                let
                    indexToLetter n =
                        Utils.stringCharAt (Debug.log "letter index" n) alphabet
                            |> Maybe.withDefault '?'
                in
                ( model
                , Random.generate
                    (indexToLetter >> RandomLetter)
                    (Random.int 0 (String.length alphabet - 1))
                )

            else
                modelCmd

        ( RandomLetter letter, GameIntro _ ) ->
            ( startGame letter model
            , Cmd.none
            )

        ( RandomLetter letter, _ ) ->
            modelCmd

        ( Inputted text, GamePlaying words _ ) ->
            if isEnter text then
                case Ticker.current model.ticker of
                    Just (Text.ActiveAthleteInput _ _) ->
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

        ( Inputted _, _ ) ->
            modelCmd

        ( Resized, _ ) ->
            ( model
            , Viewport.get
            )

        ( GotViewport viewport, _ ) ->
            ( { model | viewport = viewport }
            , Cmd.none
            )

        ( NoOp, _ ) ->
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
                , Font.size Palette.textSizeLarger
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

        passed =
            Ticker.passed model.ticker
                |> List.map tickerText

        tickerTexts =
            (case Ticker.current model.ticker of
                Just cur ->
                    tickerActive cur :: passed

                Nothing ->
                    passed
            )
                |> List.reverse
                |> List.intersperse (text " ")
    in
    el
        [ clip
        , width fill
        , height (px Palette.textSizeLarger)
        , centerY
        , Font.size Palette.textSizeLarger
        , inFront input
        ]
        (row
            [ alignRight ]
            tickerTexts
        )


tickerActive : Text.Active -> Element Msg
tickerActive ta =
    case ta of
        Text.ActiveAnnouncement txt ticks ->
            text (String.left ticks (String.toUpper (Paragraph.toString txt)))

        Text.ActiveInstruction txt ticks ->
            text (String.left ticks (String.toUpper (Paragraph.toString txt)))

        Text.ActiveAthleteInput txt _ ->
            text (String.toUpper txt)


tickerText : Text.Text -> Element Msg
tickerText tt =
    case tt of
        Text.InterruptedAnnouncement txt ticks ->
            text <| String.left ticks (String.toUpper (Paragraph.toString txt)) ++ "—"

        Text.FinishedAnnouncement txt ->
            text (String.toUpper (Paragraph.toString txt))

        Text.Instruction txt ->
            text (String.toUpper (Paragraph.toString txt))

        Text.CorrectAthleteInput txt ->
            text (String.toUpper txt ++ "🙆")

        Text.WrongAthleteInput txt ->
            text (String.toUpper txt ++ "🙅")


type Athlete
    = AthleteA
    | AthleteB


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize <|
            \w h -> Resized
        , Viewport.got GotViewport NoOp
        , Time.every 200 Ticked
        ]



-- OTHER


alphabet =
    "abcdefghijklmnopqrstuvwxyz"


startGame : Char -> Model -> Model
startGame initial model =
    case model.game of
        GameIntro words ->
            { model
                | game = GamePlaying words JustStarted
                , ticker =
                    model.ticker
                        |> Ticker.queueUp (Text.QueuedInstruction (paragraphFromString ("Try “" ++ String.fromChar initial ++ "”!")))
                        |> Ticker.queueUp (Text.QueuedAthleteInput (Constraints.serve initial))
            }

        _ ->
            model


checkPartialInput : Words -> Model -> Model
checkPartialInput words model =
    case Ticker.current model.ticker of
        Just (Text.ActiveAthleteInput txt cnts) ->
            case Constraints.checkCandidate txt cnts words of
                Constraints.CandidateCorrect ->
                    model

                Constraints.CandidateInitialWrong ->
                    inputWrong model

                Constraints.CandidateNotAWord ->
                    inputWrong model

        Just _ ->
            model

        Nothing ->
            model


checkInput : Words -> Model -> Model
checkInput words model =
    case Ticker.current model.ticker of
        Just (Text.ActiveAthleteInput txt cnts) ->
            case Constraints.check txt cnts words of
                Constraints.InputCorrect ->
                    inputCorrect model

                Constraints.InputInitialWrong ->
                    inputWrong model

                Constraints.InputIncorporatesWrong ->
                    inputWrong model

                Constraints.InputNotAWord ->
                    inputWrong model

        _ ->
            model


inputCorrect : Model -> Model
inputCorrect model =
    case Ticker.current model.ticker of
        Just (Text.ActiveAthleteInput previousWord cnts) ->
            let
                newCnts =
                    Constraints.rally
                        { initial = Constraints.getInitial cnts
                        , incorporates = Utils.stringLast previousWord |> Maybe.withDefault '?'
                        }
            in
            { model
                | ticker =
                    Ticker.inputCorrect model.ticker
                        |> Ticker.queueUp (Text.QueuedAnnouncement (paragraphFromString "Good move! Try another!"))
                        |> Ticker.queueUp (Text.QueuedAthleteInput newCnts)
            }

        _ ->
            model


inputWrong : Model -> Model
inputWrong model =
    case Ticker.current model.ticker of
        Just (Text.ActiveAthleteInput previousWord cnts) ->
            let
                newCnts =
                    case Constraints.getIncorporates cnts of
                        Just incorporates ->
                            Constraints.rally
                                { initial = Constraints.getInitial cnts
                                , incorporates = incorporates
                                }

                        Nothing ->
                            Constraints.serve (Constraints.getInitial cnts)
            in
            { model
                | ticker =
                    Ticker.inputWrong model.ticker
                        |> Ticker.queueUp (Text.QueuedAnnouncement (paragraphFromString "Too bad! Try again!"))
                        |> Ticker.queueUp (Text.QueuedAthleteInput newCnts)
            }

        _ ->
            model


isEnter text =
    text == "\n"


paragraphFromString : String -> Paragraph.Paragraph
paragraphFromString str =
    Paragraph.create [ Doc.Text.create Doc.Format.empty str ]
