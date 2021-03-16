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
    { ticker : List TickerStatus
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


type TickerStatus
    = TickingTicker String Int
    | InterruptedTicker String Int
    | FinishedTicker String


type InputStatus
    = Correct
    | Incorrect


type alias Flags =
    { viewport : Viewport
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { ticker = [ TickingTicker "Spwords!" 0, InterruptedTicker "My name is Ale" 5, FinishedTicker "Go!" ]
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
                (TickingTicker text ticks) :: rest ->
                    if ticks < String.length text then
                        ( { model | ticker = TickingTicker text (ticks + 1) :: rest }
                        , doLetterTick
                        )

                    else
                        ( { model | ticker = FinishedTicker text :: rest }
                        , Cmd.none
                        )

                _ ->
                    modelMsg

        Interrupted ->
            case model.ticker of
                (TickingTicker text ticks) :: rest ->
                    ( { model | ticker = InterruptedTicker text ticks :: rest }
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
    el
        [ clip
        , width fill
        , centerY
        , Font.size Palette.textSizeLarger
        ]
        (row
            [ alignRight ]
            (model.ticker |> List.map tickerAnnouncement |> List.reverse |> List.intersperse (text " "))
        )


tickerAnnouncement : TickerStatus -> Element Msg
tickerAnnouncement t =
    case t of
        TickingTicker txt ticks ->
            text <| String.left ticks txt

        InterruptedTicker txt ticks ->
            text <| String.left ticks txt ++ "--"

        FinishedTicker txt ->
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
