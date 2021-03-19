module Main exposing (..)

import Browser
import Browser.Events
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
import Return as R exposing (Return)
import Task
import Texts
import Ticker exposing (Ticker)
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
    , words : GameStatus
    , viewport : Viewport
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
            Ticker.empty |> Ticker.queueUp (Text.QueuedInstruction "(Loading…)")
      , words = GameLoading
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
    | Resized
    | GotViewport Viewport
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        modelCmd =
            ( model, Cmd.none )
    in
    case ( msg, model.words ) of
        ( GotWords result, GameLoading ) ->
            case result of
                Ok words ->
                    ( { model
                        | words = GameIntro (Words.parse words)
                        , ticker =
                            model.ticker
                                |> Ticker.queueUp (Text.QueuedAnnouncement "(Done. Press Enter.)")
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | words = WordsLoadError err }
                    , Cmd.none
                    )

        ( GotWords _, _ ) ->
            modelCmd

        ( Ticked _, _ ) ->
            ( { model | ticker = Ticker.tick model.ticker }
            , Cmd.none
            )

        ( Inputted text, GameIntro words ) ->
            if isEnter text then
                ( { model
                    | words = GamePlaying words
                    , ticker =
                        model.ticker
                            |> Ticker.queueUp (Text.QueuedInstruction "Try a word with \"S\"!")
                            |> Ticker.queueUp (Text.QueuedAthleteInput (Constraints.Serve { initial = 's' }))
                  }
                , Cmd.none
                )

            else
                modelCmd

        ( Inputted text, GamePlaying words ) ->
            if isEnter text then
                case Ticker.current model.ticker of
                    Just (Text.ActiveAthleteInput txt cnts) ->
                        ( { model
                            | ticker =
                                if inputIsCorrect txt cnts words then
                                    Ticker.inputCorrect model.ticker
                                        |> Ticker.queueUp (Text.QueuedAnnouncement "Good move!")

                                else
                                    Ticker.inputWrong model.ticker
                                        |> Ticker.queueUp (Text.QueuedAnnouncement "Too bad! That didn't go well.")
                          }
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
                , Cmd.none
                )
                    |> R.map (checkPartialInput words)

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
            text (String.left ticks txt)

        Text.ActiveInstruction txt ticks ->
            text (String.left ticks txt)

        Text.ActiveAthleteInput txt _ ->
            text txt


tickerText : Text.Text -> Element Msg
tickerText tt =
    case tt of
        Text.InterruptedAnnouncement txt ticks ->
            text <| String.left ticks txt ++ "--"

        Text.FinishedAnnouncement txt ->
            text txt

        Text.Instruction txt ->
            text txt

        Text.CorrectAthleteInput txt ->
            text (txt ++ "🙆")

        Text.WrongAthleteInput txt ->
            text (txt ++ "🙅")


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


checkPartialInput : Words -> Model -> Model
checkPartialInput words model =
    case Ticker.current model.ticker of
        Just (Text.ActiveAthleteInput txt cnts) ->
            if not (inputIsCandidate txt cnts words) then
                { model | ticker = Ticker.inputWrong model.ticker }

            else
                model

        Just _ ->
            model

        Nothing ->
            model


inputIsCandidate : String -> Constraints -> Words -> Bool
inputIsCandidate text cnts words =
    let
        initial_ =
            case cnts of
                Constraints.Serve { initial } ->
                    initial

                Constraints.Rally { initial } ->
                    initial
    in
    case Utils.stringHead text of
        Just head ->
            (head == initial_)
                && Words.candidate text words

        Nothing ->
            True


inputIsCorrect : String -> Constraints -> Words -> Bool
inputIsCorrect text cnts words =
    let
        initial_ =
            case cnts of
                Constraints.Serve { initial } ->
                    initial

                Constraints.Rally { initial } ->
                    initial
    in
    case Utils.stringHead text of
        Just head ->
            (head == initial_)
                && Words.exists text words

        Nothing ->
            True


isEnter text =
    text == "\n"
