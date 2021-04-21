module Main exposing (..)

import Athlete exposing (..)
import Browser
import Browser.Events
import Constraints exposing (Constraints)
import Dict exposing (Dict)
import Doc.Format
import Doc.Paragraph as Paragraph exposing (Paragraph)
import Doc.Text
import Doc.Util
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Cursor as Cursor
import Element.Custom.Attribute as Attribute
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Game exposing (Game, GameMode(..))
import Game.Times as Times exposing (Times)
import Html exposing (Html)
import Http
import Layout exposing (Layout)
import Levers
import Maybe.Extra as Maybe
import Palette
import Random
import Score exposing (..)
import Speed exposing (Speed)
import Texts
import Ticker.Active as Active exposing (Active)
import Ticker.Announcement as Announcement exposing (Announcement)
import Ticker.Message as Message exposing (Message)
import Ticker.Passed as Passed exposing (Passed)
import Time
import Util exposing (fraction, ifElse)
import Util.Element exposing (toCssColor)
import Util.List exposing (appendWhen, consMaybe, consWhen)
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
    { status : Status
    , gameMode : Game.GameMode
    , speed : Speed
    , inputFocused : Bool
    , layout : Layout
    , randomSeed : Random.Seed
    }


type Status
    = Loading Announcement
    | Ready Words Passed Announcement
    | Playing Words Passed Game
    | WordsLoadError Http.Error Passed Announcement



-- INIT


type alias Flags =
    { viewport : Viewport
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { status = Loading (Announcement.create Texts.loading)
      , gameMode = Game.SingleMode
      , speed = Speed.Normal
      , inputFocused = False
      , layout = Layout.fromViewport flags.viewport
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
    | SelectedMode Game.GameMode
    | SelectedSpeed Speed
    | SelectedRestart
    | InputFocusChange Bool
    | InputSelected
    | Resized
    | GotViewport Viewport
    | GotSeed Random.Seed
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ignore =
            ( model, Cmd.none )
    in
    case msg of
        Ticked _ ->
            ( tickStatus model
            , Cmd.none
            )

        Inputted input ->
            if isEnter input then
                ( pressedEnter model
                , Cmd.none
                )

            else
                case model.status of
                    Playing words passed game ->
                        let
                            ( newGame, newSeed, messageM ) =
                                Game.userInput input model.randomSeed words game

                            newPassed =
                                case messageM of
                                    Just message ->
                                        passed |> Passed.push message

                                    Nothing ->
                                        passed
                        in
                        ( { model
                            | status = Playing words newPassed newGame
                            , randomSeed = newSeed
                          }
                        , Cmd.none
                        )

                    _ ->
                        ignore

        SelectedMode mode ->
            ( { model | gameMode = mode }
            , Cmd.none
            )

        SelectedSpeed speed ->
            ( { model | speed = speed }
            , Cmd.none
            )

        SelectedRestart ->
            case model.status of
                Playing words _ _ ->
                    ( { model | status = ready words Passed.empty }
                    , Cmd.none
                    )

                _ ->
                    ignore

        InputFocusChange focused ->
            let
                newModel =
                    { model | inputFocused = focused }
            in
            case model.status of
                Ready _ _ _ ->
                    ( startPlay newModel, Cmd.none )

                _ ->
                    ( newModel, Cmd.none )

        InputSelected ->
            case model.status of
                Ready _ _ _ ->
                    ( startPlay model, Cmd.none )

                _ ->
                    ignore

        -- INITIALIZATION STAGE
        --
        GotSeed seed ->
            ( { model | randomSeed = seed }
            , Cmd.none
            )

        GotWords result ->
            ( gotWords result model
            , Cmd.none
            )

        -- OTHERS
        --
        Resized ->
            ( model
            , Viewport.get
            )

        GotViewport viewport ->
            ( { model | layout = Layout.fromViewport viewport }
            , Cmd.none
            )

        NoOp ->
            ignore


gotWords : Result Http.Error String -> Model -> Model
gotWords result model =
    case model.status of
        Loading ann ->
            case result of
                Ok words ->
                    { model | status = ready (Words.parse words) (Passed.singleton (Announcement.toMessage ann)) }

                Err err ->
                    { model
                        | status =
                            WordsLoadError err
                                (Passed.singleton (Announcement.toMessage ann))
                                (Announcement.create Texts.loadError)
                    }

        _ ->
            model


ready : Words -> Passed -> Status
ready words passed =
    Ready words passed (Announcement.create Texts.ready)


startPlay : Model -> Model
startPlay model =
    case model.status of
        Ready words passed ann ->
            { model | status = Playing words (Passed.pushAnnouncement ann passed) (Game.startGame model.gameMode) }

        _ ->
            model


tickStatus : Model -> Model
tickStatus model =
    case model.status of
        Loading ann ->
            { model | status = Loading (Announcement.tick ann) }

        WordsLoadError err passed ann ->
            { model | status = WordsLoadError err passed (Announcement.tick ann) }

        Ready words passed ann ->
            { model | status = Ready words passed (Announcement.tick ann) }

        Playing words passed game ->
            let
                ( newGame, newSeed, messageM ) =
                    Game.tick model.randomSeed words game

                newPassed =
                    case messageM of
                        Just message ->
                            passed |> Passed.push message

                        Nothing ->
                            passed
            in
            { model
                | status = Playing words newPassed newGame
                , randomSeed = newSeed
            }


pressedEnter : Model -> Model
pressedEnter model =
    case model.status of
        -- Ready words passed ann ->
        --     { model | status = Playing words (Passed.pushAnnouncement ann passed) (Game.startGame model.gameMode) }
        Playing words passed game ->
            let
                ( newGame, newSeed, messageM ) =
                    Game.skip model.randomSeed words game

                newPassed =
                    case messageM of
                        Just message ->
                            passed |> Passed.push message

                        Nothing ->
                            passed
            in
            { model
                | status = Playing words newPassed newGame
                , randomSeed = newSeed
            }

        _ ->
            model



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
            , Font.size (Palette.textSizeNormal model.layout)
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
            getActiveAthlete model.status

        isAthlete athlete =
            case activeAthlete of
                Just ath ->
                    athlete == ath

                _ ->
                    False

        times =
            case model.status of
                Playing _ _ game ->
                    Game.getTimes game

                _ ->
                    Times.start
    in
    column [ height fill, width fill ]
        [ bar model.layout AthleteA (Times.get AthleteA times) (isAthlete AthleteA)
        , ticker model
        , bar model.layout AthleteB (Times.get AthleteB times) (isAthlete AthleteB)
        ]


ticker : Model -> Element Msg
ticker model =
    let
        cursor =
            el
                [ width (px (Palette.spaceSmall model.layout))
                , height (px (Palette.textSizeLarge model.layout * 2))
                , Background.color <|
                    case getActiveAthlete model.status of
                        Just athlete ->
                            athleteColor athlete

                        Nothing ->
                            Palette.transparent
                ]
                none

        toTickerTexts act passed =
            (act
                :: tickerPassed passed
            )
                |> List.reverse

        gameEnded =
            case model.status of
                Playing _ _ game ->
                    Game.ended game

                _ ->
                    False

        athleteM =
            case model.status of
                Playing _ _ game ->
                    Game.getActiveAthlete game
                        |> Maybe.filter (isHumanAthlete model.gameMode)

                _ ->
                    Nothing

        tickerEl act passed =
            row
                ([ centerY
                 , width fill
                 , Cursor.default
                 ]
                    |> consWhen (not (playing model.status))
                        (above (title model.layout model.gameMode model.speed gameEnded))
                )
                [ el
                    [ inFront (inputEl model.layout model.inputFocused athleteM)
                    , Font.size (Palette.textSizeLarge model.layout)
                    , height (px (fraction 1.2 (Palette.textSizeLarge model.layout)))
                    , width fill
                    , clip
                    ]
                    (row
                        [ alignRight, centerY ]
                        (toTickerTexts act passed)
                    )
                , cursor
                ]
    in
    case model.status of
        Loading ann ->
            tickerEl (tickerAnnouncement ann) Passed.empty

        WordsLoadError _ passed ann ->
            tickerEl (tickerAnnouncement ann) passed

        Ready _ passed ann ->
            tickerEl (tickerAnnouncement ann) passed

        Playing _ passed game ->
            tickerEl (tickerActive (Game.getActive game)) passed


inputEl : Layout -> Bool -> Maybe Athlete -> Element Msg
inputEl layout inputFocused athleteM =
    let
        pressHere =
            case athleteM of
                Just athlete ->
                    if not inputFocused then
                        el
                            [ Background.color (athleteColorTransparent athlete)
                            , Font.color Palette.dark
                            , Font.size (Palette.textSizeNormal layout)
                            , Font.bold
                            , Font.center
                            , width fill
                            , height fill
                            ]
                            (el [ centerY, centerX ]
                                (text "PRESS HERE")
                            )

                    else
                        el [] none

                Nothing ->
                    el [] none
    in
    Input.multiline
        [ Events.onFocus (InputFocusChange True)
        , Events.onLoseFocus (InputFocusChange False)
        , Events.onClick InputSelected
        , behindContent pressHere
        , Font.size 5
        , Font.color Palette.transparent
        , Background.color Palette.transparent
        , width fill
        , height fill
        , padding 0
        , Border.width 0
        , focused [ Border.glow Palette.transparent 0 ]
        , Border.rounded 0
        , Cursor.default
        ]
        { text = ""
        , onChange = Inputted
        , placeholder = Nothing
        , label = Input.labelHidden ""
        , spellcheck = False
        }


title : Layout -> Game.GameMode -> Speed -> Bool -> Element Msg
title layout gameMode speed ended =
    let
        nextMode =
            case gameMode of
                HotseatMode ->
                    SingleMode

                SingleMode ->
                    HotseatMode

        nextSpeed =
            case speed of
                Speed.Normal ->
                    Speed.Slow

                Speed.Slow ->
                    Speed.Normal

        titleText =
            [ el [ Font.bold ] (text "SPWORDS")
            , text " BY "
            , newTabLink [] { label = text "AGJ", url = "http://agj.cl" }
            , text ". "
            ]

        modeSelection =
            [ el
                [ Events.onClick (SelectedMode nextMode)
                , Cursor.pointer
                ]
                (text
                    (case gameMode of
                        HotseatMode ->
                            "[2P HOTSEAT]"

                        SingleMode ->
                            "[SOLO]"
                    )
                )
            , text " MODE. "
            ]

        speedSelection =
            [ el
                [ Events.onClick (SelectedSpeed nextSpeed)
                , Cursor.pointer
                ]
                (text
                    (case speed of
                        Speed.Normal ->
                            "[TOURNAMENT]"

                        Speed.Slow ->
                            "[AMATEUR]"
                    )
                )
            , text " SPEED. "
            ]

        restart =
            [ el
                [ Events.onClick SelectedRestart
                , Cursor.pointer
                ]
                (text "[RESTART]")
            , text " "
            ]

        optionsOrRestart =
            List.map (row [ alignRight, Font.color Palette.lightish ]) <|
                if ended then
                    [ restart ]

                else
                    [ modeSelection
                    , speedSelection
                    ]
    in
    case layout of
        _ ->
            column
                [ alignRight
                , Cursor.default
                , moveDown (1.5 * toFloat (Palette.textSizeNormal layout))
                , spacing (Palette.textLineSpacing (Palette.textSizeNormal layout))
                ]
                ([ row [ alignRight ] titleText ] ++ optionsOrRestart)


tickerActive : Maybe Active -> Element Msg
tickerActive activeM =
    case activeM of
        Just (Active.AthleteInput athlete input) ->
            el
                [ Font.color (athleteColor athlete)
                , Font.underline
                , Font.bold
                ]
                (text (String.toUpper input))

        Just (Active.Announcement ann) ->
            tickerAnnouncement ann

        Nothing ->
            none


tickerAnnouncement : Announcement -> Element Msg
tickerAnnouncement ann =
    Announcement.getCurrent ann
        |> fromDocParagraph


tickerPassed : Passed -> List (Element Msg)
tickerPassed passed =
    Passed.toList passed
        |> List.map tickerMessage


tickerMessage : Message -> Element Msg
tickerMessage tt =
    case tt of
        Message.InterruptedAnnouncement txt ticks ->
            fromDocParagraph (Doc.Util.paragraphAppend "—" (Doc.Util.paragraphLeft ticks txt))

        Message.FinishedAnnouncement txt ->
            fromDocParagraph txt

        Message.CorrectAthleteInput athlete txt ->
            let
                txtUpper =
                    String.toUpper txt

                color =
                    athleteColor athlete
            in
            row
                [ Font.color color
                , Font.bold
                ]
                [ text (String.dropRight 1 txtUpper)
                , el
                    [ Font.color Palette.dark
                    , Background.color color
                    ]
                    (text (String.right 1 txtUpper))
                , el [ Font.regular ]
                    (text "✔")
                ]

        Message.WrongAthleteInput athlete txt ->
            el
                [ Font.color (athleteColor athlete)
                , Font.strike
                , Font.bold
                , Attribute.style "text-decoration-color" (toCssColor Palette.light)
                ]
                (text (String.toUpper txt))


bar : Layout -> Athlete -> Float -> Bool -> Element Msg
bar layout athlete timeLeft active =
    let
        filledPortion =
            round (timeLeft * 10000)

        emptyPortion =
            10000 - filledPortion
    in
    el
        [ width fill
        , height (px (Palette.spaceLarge layout))
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
                    px (Palette.spaceLarge layout)

                 else
                    px (Palette.spaceNormal layout)
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
    []
        |> consWhen (Doc.Format.isBold format)
            Font.bold
        |> consWhen (Doc.Format.isItalic format)
            Font.italic
        |> consMaybe
            (Doc.Format.athlete format
                |> Maybe.map
                    (\athlete ->
                        Font.color (athleteColor athlete)
                    )
            )


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


athleteColorTransparent : Athlete -> Color
athleteColorTransparent athlete =
    case athlete of
        AthleteA ->
            Palette.athleteATransparent

        AthleteB ->
            Palette.athleteBTransparent



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize <|
            \w h -> Resized
        , Viewport.got GotViewport NoOp
        , Time.every (Levers.tickInterval model.speed) Ticked
        ]



-- OTHER


isEnter text =
    text == "\n"


getActiveAthlete : Status -> Maybe Athlete
getActiveAthlete status =
    case status of
        Playing _ _ game ->
            Game.getActiveAthlete game

        _ ->
            Nothing


isHumanAthlete : GameMode -> Athlete -> Bool
isHumanAthlete mode athlete =
    mode == HotseatMode || (mode == SingleMode && athlete == AthleteA)


playing : Status -> Bool
playing status =
    case status of
        Playing _ _ game ->
            not <| Game.ended game

        _ ->
            False
