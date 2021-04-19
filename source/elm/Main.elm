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
import Html exposing (Html)
import Http
import Levers
import Palette
import Random
import Score exposing (..)
import Texts
import Ticker.Active as Active exposing (Active)
import Ticker.Announcement as Announcement exposing (Announcement)
import Ticker.Message as Message exposing (Message)
import Ticker.Passed as Passed exposing (Passed)
import Time
import Util exposing (ifElse)
import Util.Element exposing (toCssColor)
import Util.List exposing (appendWhen, consWhen)
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
    , viewport : Viewport
    , randomSeed : Random.Seed
    }


type Status
    = Loading Announcement
    | Ready Words Passed Announcement
    | Playing Words Passed Game
    | WordsLoadError Http.Error



-- INIT


type alias Flags =
    { viewport : Viewport
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { status = Loading (Announcement.create Texts.loading)
      , gameMode = Game.HotseatMode
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
    | SelectedMode Game.GameMode
    | Resized
    | GotViewport Viewport
    | GotSeed Random.Seed
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        default =
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
                        default

        SelectedMode mode ->
            ( { model | gameMode = mode }
            , Cmd.none
            )

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
            ( { model | viewport = viewport }
            , Cmd.none
            )

        NoOp ->
            default


gotWords : Result Http.Error String -> Model -> Model
gotWords result model =
    case model.status of
        Loading ann ->
            case result of
                Ok words ->
                    { model
                        | status =
                            Ready (Words.parse words)
                                (Passed.empty |> Passed.push (Announcement.toMessage ann))
                                (Announcement.create Texts.ready)
                    }

                Err err ->
                    { model | status = WordsLoadError err }

        _ ->
            model


tickStatus : Model -> Model
tickStatus model =
    case model.status of
        Loading ann ->
            { model | status = Loading (Announcement.tick ann) }

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

        _ ->
            model


pressedEnter : Model -> Model
pressedEnter model =
    case model.status of
        Ready words passed ann ->
            { model | status = Playing words (Passed.pushAnnouncement ann passed) (Game.startGame model.gameMode) }

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
    let
        activeAthlete =
            getActiveAthlete model.status

        isAthlete athlete =
            case activeAthlete of
                Just ath ->
                    athlete == ath

                _ ->
                    False
    in
    column [ height fill, width fill ]
        [ bar AthleteA (TimeLeft 0.5) (isAthlete AthleteA)
        , ticker model
        , bar AthleteB (TimeLeft 0.5) (isAthlete AthleteB)
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
                , Cursor.default
                ]
                { text = ""
                , onChange = Inputted
                , placeholder = Nothing
                , label = Input.labelHidden ""
                , spellcheck = False
                }

        cursor =
            el
                [ width (px 5)
                , height (px (Palette.textSizeLarger * 2))
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
                |> List.intersperse (text " ")

        gameEnded =
            case model.status of
                Playing _ _ game ->
                    Game.ended game

                _ ->
                    False

        tickerEl act passed =
            row
                ([ centerY
                 , width fill
                 , Cursor.default
                 ]
                    |> consWhen (not <| playing model.status)
                        (above (title model.gameMode gameEnded))
                )
                [ el
                    [ clip
                    , width fill
                    , height (px Palette.textSizeLarger)
                    , Font.size Palette.textSizeLarger
                    , inFront input
                    ]
                    (row
                        [ alignRight ]
                        (toTickerTexts act passed)
                    )
                , cursor
                ]
    in
    case model.status of
        Loading ann ->
            tickerEl (tickerAnnouncement ann) Passed.empty

        Ready _ passed ann ->
            tickerEl (tickerAnnouncement ann) passed

        Playing _ passed game ->
            tickerEl (tickerActive (Game.getActive game)) passed

        WordsLoadError err ->
            none


title : Game.GameMode -> Bool -> Element Msg
title gameMode ended =
    let
        nextMode =
            case gameMode of
                HotseatMode ->
                    SingleMode

                SingleMode ->
                    HotseatMode

        options =
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
            , text " MODE. NORMAL SPEED. "
            ]

        restart =
            [ el
                [ Events.onClick NoOp
                , Cursor.pointer
                ]
                (text "[RESTART]")
            ]
    in
    row
        [ Font.size Palette.textSizeLarge
        , alignRight
        , Cursor.default
        , moveDown (1.7 * toFloat Palette.textSizeLarge)
        ]
        ([ el [ Font.bold ] (text "SPWORDS")
         , text " BY "
         , newTabLink [] { label = text "AGJ", url = "http://agj.cl" }
         , text ". "
         ]
            ++ ifElse ended
                restart
                options
        )


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


type TimeLeft
    = TimeLeft Float


bar : Athlete -> TimeLeft -> Bool -> Element Msg
bar athlete (TimeLeft timeLeft) active =
    let
        filledPortion =
            round (timeLeft * 10000)

        emptyPortion =
            10000 - filledPortion

        fullHeight =
            Palette.spaceSmall * 3
    in
    el
        [ width fill
        , height (px fullHeight)
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
                    px fullHeight

                 else
                    px Palette.spaceSmall
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
            Just athlete ->
                [ Font.color (athleteColor athlete) ]

            Nothing ->
                []
        ]


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


isEnter text =
    text == "\n"


getActiveAthlete : Status -> Maybe Athlete
getActiveAthlete status =
    case status of
        Playing _ _ game ->
            Game.getActiveAthlete game

        _ ->
            Nothing


playing : Status -> Bool
playing status =
    case status of
        Playing _ _ game ->
            not <| Game.ended game

        _ ->
            False
