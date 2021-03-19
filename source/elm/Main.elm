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
import Ticker.Queued as Queued
import Ticker.Text
import Ticker.Text.AthleteInput as AthleteInput exposing (AthleteInput)
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
    , constraints : Constraints
    , viewport : Viewport
    }


type GameStatus
    = GameLoading
    | GameIntro Words
    | GamePlaying Words
    | WordsLoadError Http.Error


type Constraints
    = ServeConstraints
        { initial : Char
        }
    | RallyConstraints
        { initial : Char
        , includes : Char
        }


type alias Flags =
    { viewport : Viewport
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { ticker =
            Ticker.empty |> Ticker.queueUp (Queued.Instruction "(Loading…)")
      , words = GameLoading
      , constraints = ServeConstraints { initial = 's' }
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
                                |> Ticker.queueUp (Queued.Announcement "(Done. Press Enter.)")
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
                            |> Ticker.queueUp (Queued.Instruction "Try a word with \"S\"!")
                            |> Ticker.queueUp Queued.AthleteInput
                            |> Ticker.queueUp (Queued.Announcement "Too bad! That didn't go well.")
                            |> Ticker.enter
                  }
                , Cmd.none
                )

            else
                modelCmd

        ( Inputted text, GamePlaying words ) ->
            if isEnter text then
                ( { model | ticker = Ticker.enter model.ticker }
                , Cmd.none
                )
                    |> R.map (checkInput model.constraints words)

            else
                ( { model | ticker = Ticker.input text model.ticker }
                , Cmd.none
                )
                    |> R.map (checkInput model.constraints words)

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
            (Ticker.toList model.ticker
                |> List.map tickerText
                |> List.reverse
                |> List.intersperse (text " ")
            )
        )


tickerText : Ticker.Text.Text -> Element Msg
tickerText tt =
    case tt of
        Ticker.Text.Announcement ta ->
            tickerAnnouncement ta

        Ticker.Text.Instruction ti ->
            tickerInstruction ti

        Ticker.Text.AthleteInput tai ->
            tickerTickerAthleteInput tai


tickerAnnouncement : Ticker.Text.Announcement -> Element Msg
tickerAnnouncement t =
    case t of
        Ticker.Text.TickingAnnouncement txt ticks ->
            text <| String.left ticks txt

        Ticker.Text.InterruptedAnnouncement txt ticks ->
            text <| String.left ticks txt ++ "--"

        Ticker.Text.FinishedAnnouncement txt ->
            text txt


tickerInstruction : Ticker.Text.Instruction -> Element Msg
tickerInstruction t =
    case t of
        Ticker.Text.TickingInstruction txt ticks ->
            text <| String.left ticks txt

        Ticker.Text.FinishedInstruction txt ->
            text txt


tickerTickerAthleteInput : AthleteInput -> Element Msg
tickerTickerAthleteInput t =
    case t of
        AthleteInput.Inputting txt ->
            text txt

        AthleteInput.Correct txt ->
            text (txt ++ "🙆")

        AthleteInput.Wrong txt ->
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


checkInput : Constraints -> Words -> Model -> Model
checkInput cnts words model =
    case Ticker.inputted model.ticker of
        Just text ->
            if not (inputIsCandidate text cnts words) then
                { model | ticker = Ticker.inputWrong model.ticker }

            else
                model

        Nothing ->
            model


inputIsCandidate : String -> Constraints -> Words -> Bool
inputIsCandidate text cnts words =
    let
        initial_ =
            case cnts of
                ServeConstraints { initial } ->
                    initial

                RallyConstraints { initial } ->
                    initial
    in
    case Utils.stringHead text of
        Just head ->
            (head == initial_)
                && Words.candidate text words

        Nothing ->
            False


isEnter text =
    text == "\n"
