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
import Palette
import Process
import Return as R exposing (Return)
import Task
import Texts
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
    { ticker : TickerAnnouncement
    , viewport : Viewport
    }



-- type Status
--     = Intro
--     | Announcement
--     | Typing
--     | MachinesTurn


type TickerText
    = Announcement TickerAnnouncement
    | AthleteInput { athlete : Athlete, input : String, status : InputStatus }


type alias TickerAnnouncement =
    { text : String, ticks : Int }


type InputStatus
    = Correct
    | Incorrect


type alias Flags =
    { viewport : Viewport
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { ticker = { text = "Spwords!", ticks = 0 }
      , viewport = flags.viewport
      }
    , doLetterTick
    )


doLetterTick : Cmd Msg
doLetterTick =
    Task.perform (always LetterTicked) (Process.sleep 200)



-- UPDATE


type Msg
    = LetterTicked
    | Resized
    | GotViewport Viewport
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        modelMsg =
            ( model, Cmd.none )

        letterTicksLens : (Int -> Int) -> Model -> Model
        letterTicksLens updater m =
            let
                t =
                    m.ticker
            in
            { m | ticker = { t | ticks = updater t.ticks } }
    in
    case msg of
        LetterTicked ->
            ( letterTicksLens (\lt -> lt + 1) model
            , if model.ticker.ticks < String.length model.ticker.text then
                doLetterTick

              else
                Cmd.none
            )

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
    el
        [ clip
        , width fill
        , centerY
        , Font.size Palette.textSizeLarger
        ]
        (row
            [ alignRight ]
            [ tickerAnnouncement model.ticker
            ]
        )


tickerAnnouncement : TickerAnnouncement -> Element Msg
tickerAnnouncement t =
    text <| String.left t.ticks t.text


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
        ]
