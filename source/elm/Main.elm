module Main exposing (..)

import Browser
import Browser.Events
import CustomEl
import Debug
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
    { game : Game
    , viewport : Viewport
    }


type alias Game =
    { message : String
    , letterTicks : Int
    }


type alias Flags =
    { viewport : { width : Int, height : Int }
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { game = { message = "Spwords", letterTicks = 0 }
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

        letterTicksLens updater m =
            let
                game =
                    m.game
            in
            { m | game = { game | letterTicks = updater game.letterTicks } }
    in
    case msg of
        LetterTicked ->
            ( letterTicksLens (\lt -> lt + 1) model
            , doLetterTick
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
            (mainScreen model.game)
        ]
    }


mainScreen : Game -> Element Msg
mainScreen game =
    column [ height fill, width fill ]
        [ bar AthleteA (TimeLeft 0.5)
        , statusDisplay game
        , bar AthleteB (TimeLeft 0.5)
        ]


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


statusDisplay : Game -> Element Msg
statusDisplay game =
    el
        [ clip
        , width fill
        , centerY
        , Font.size Palette.textSizeLarger
        ]
        (el
            [ alignRight ]
            (text (String.left game.letterTicks game.message))
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize <|
            \w h -> Resized
        , Viewport.got GotViewport NoOp
        ]
