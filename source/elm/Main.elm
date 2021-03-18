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
import Json.Decode as Decode
import Palette
import Process
import Return as R exposing (Return)
import Task
import Texts
import Ticker exposing (Ticker)
import Ticker.Queued as Queued
import Ticker.Text
import Time
import Utils exposing (..)
import Viewport exposing (Viewport)



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
    , viewport : Viewport
    }



-- type Status
--     = Intro
--     | Announcement
--     | Typing
--     | MachinesTurn
-- type InputStatus
--     = Correct
--     | Incorrect


type alias Flags =
    { viewport : Viewport
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { ticker =
            Ticker.empty
                |> Ticker.queueUp (Queued.Announcement "Welcome to Spwords!")
                |> Ticker.queueUp (Queued.Announcement "Try a word with \"S\"!")
                |> Ticker.queueUp Queued.AthleteInput
                |> Ticker.queueUp (Queued.Announcement "Too bad! That didn't go well.")
      , viewport = flags.viewport
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Ticked Time.Posix
    | TextEntered String
    | Resized
    | GotViewport Viewport
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        modelMsg =
            ( model, Cmd.none )
    in
    case msg of
        Ticked _ ->
            ( { model | ticker = Ticker.tick model.ticker }
            , Cmd.none
            )

        TextEntered enteredText ->
            ( { model | ticker = Ticker.input enteredText model.ticker }
            , Cmd.none
            )
                |> R.map checkInput

        Resized ->
            ( model
            , Viewport.get
            )

        GotViewport viewport ->
            ( { model | viewport = viewport }
            , Cmd.none
            )

        NoOp ->
            modelMsg



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
                , onChange = TextEntered
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


tickerTickerAthleteInput : Ticker.Text.AthleteInput -> Element Msg
tickerTickerAthleteInput t =
    case t of
        Ticker.Text.InputtingAthleteInput txt ->
            text txt

        Ticker.Text.CorrectAthleteInput txt ->
            text (txt ++ "🙆")

        Ticker.Text.WrongAthleteInput txt ->
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


checkInput : Model -> Model
checkInput model =
    case Ticker.inputted model.ticker of
        Just text ->
            if isWrong text then
                { model | ticker = Ticker.inputWrong model.ticker }

            else
                model

        Nothing ->
            model


dictionary =
    [ "stadium"
    , "state"
    , "store"
    , "stain"
    ]
        |> List.map String.toUpper


isWrong text =
    dictionary
        |> List.all
            (\w ->
                String.left (String.length text) w /= text
            )
