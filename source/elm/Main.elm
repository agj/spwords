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
    { ticker : List TickerText
    , viewport : Viewport
    }



-- type Status
--     = Intro
--     | Announcement
--     | Typing
--     | MachinesTurn


type TickerText
    = Announcement TickerAnnouncement
    | AthleteInput TickerAthleteInput


type TickerAnnouncement
    = TickingTicker String Int
    | InterruptedTicker String Int
    | FinishedTicker String


type TickerAthleteInput
    = InputtingTicker String



-- type InputStatus
--     = Correct
--     | Incorrect


type alias Flags =
    { viewport : Viewport
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { ticker =
            [ AthleteInput (InputtingTicker "eh")
            , Announcement (TickingTicker "Spwords!" 0)
            , Announcement (InterruptedTicker "My name is Ale" 5)
            , Announcement (FinishedTicker "Go!")
            ]
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
    | Interrupted
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
        LetterTicked ->
            case model.ticker of
                (Announcement (TickingTicker text ticks)) :: rest ->
                    if ticks < String.length text then
                        ( { model | ticker = Announcement (TickingTicker text (ticks + 1)) :: rest }
                        , doLetterTick
                        )

                    else
                        ( { model | ticker = Announcement (FinishedTicker text) :: rest }
                        , Cmd.none
                        )

                _ ->
                    modelMsg

        Interrupted ->
            case model.ticker of
                (Announcement (TickingTicker text ticks)) :: rest ->
                    ( { model | ticker = Announcement (InterruptedTicker text ticks) :: rest }
                    , Cmd.none
                    )

                _ ->
                    modelMsg

        TextEntered txt ->
            let
                fixedText =
                    txt
                        |> String.toUpper
                        |> String.filter
                            (\ch ->
                                String.any ((==) ch) "ABCDEFGHIJKLMNOPQRSTUVWXYZÑ-'"
                            )
            in
            case model.ticker of
                (AthleteInput (InputtingTicker text)) :: rest ->
                    ( { model | ticker = AthleteInput (InputtingTicker (text ++ fixedText)) :: rest }
                    , Cmd.none
                    )

                _ ->
                    modelMsg

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
            Input.text
                [ width fill
                , Font.size Palette.textSizeLarger
                , Background.color Palette.transparent
                , Border.color Palette.transparent
                ]
                { text = ""
                , onChange = TextEntered
                , placeholder = Nothing
                , label = Input.labelHidden ""
                }
    in
    el
        [ clip
        , width fill
        , centerY
        , Font.size Palette.textSizeLarger
        , inFront input
        ]
        (row
            [ alignRight ]
            (model.ticker
                |> List.map tickerText
                |> List.reverse
                |> List.intersperse (text " ")
            )
        )


tickerText : TickerText -> Element Msg
tickerText tt =
    case tt of
        Announcement ta ->
            tickerAnnouncement ta

        AthleteInput tai ->
            tickerTickerAthleteInput tai


tickerAnnouncement : TickerAnnouncement -> Element Msg
tickerAnnouncement t =
    case t of
        TickingTicker txt ticks ->
            text <| String.left ticks txt

        InterruptedTicker txt ticks ->
            text <| String.left ticks txt ++ "--"

        FinishedTicker txt ->
            text txt


tickerTickerAthleteInput : TickerAthleteInput -> Element Msg
tickerTickerAthleteInput t =
    case t of
        InputtingTicker txt ->
            text txt


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
        , Browser.Events.onKeyDown (keyDecoder "Enter" Interrupted)
        ]


keyDecoder : String -> Msg -> Decode.Decoder Msg
keyDecoder key msg =
    Decode.field "key" Decode.string
        |> Decode.map
            (\k ->
                if k == key then
                    msg

                else
                    NoOp
            )
