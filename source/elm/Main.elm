module Main exposing (..)

import Athlete exposing (..)
import Browser
import Browser.Dom as Dom
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
import Element.Custom.Input as Input
import Element.Events as Events
import Element.Events.Pointer as Pointer
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Game exposing (Game)
import Game.GameMode exposing (GameMode(..))
import Game.Times as Times exposing (Times)
import Html exposing (Html)
import Http
import Js
import Json.Decode as Decode exposing (Value)
import Layout exposing (Layout(..))
import Levers
import Maybe.Extra as Maybe
import Menu exposing (Menu)
import Menu.MenuAction as MenuAction exposing (MenuAction)
import Menu.MenuLine as MenuLine exposing (MenuLine)
import Menu.MenuText as MenuText exposing (MenuText, MenuTextOptions)
import Palette
import Random
import SaveState
import Score exposing (..)
import Simple.Transition as Transition
import Speed exposing (Speed)
import Task exposing (Task)
import Texts
import Ticker.Active as Active exposing (Active)
import Ticker.Announcement as Announcement exposing (Announcement)
import Ticker.Message as Message exposing (Message)
import Ticker.Passed as Passed exposing (Passed)
import Time
import Util exposing (fraction, ifElse)
import Util.Element exposing (sides, toCssColor)
import Util.Element.Transition as Transition
import Util.List as List exposing (appendWhen, consMaybe, consWhen)
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
    , menu : Menu
    , inputFocused : Bool
    , layout : Layout
    , height : Int
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
    , saveState : Value
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        { mode, speed } =
            flags.saveState
                |> Decode.decodeValue SaveState.decoder
                |> Result.withDefault { mode = SingleMode, speed = Speed.Normal }
    in
    ( { status = Loading (Announcement.create Texts.loading)
      , menu =
            Menu.start mode speed
      , inputFocused = False
      , layout = Layout.fromViewport flags.viewport
      , height = flags.viewport.height
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
    | TickedMenu Time.Posix
    | Inputted String
    | GotWords (Result Http.Error String)
    | SelectedMode GameMode
    | SelectedSpeed Speed
    | SelectedRestart
    | InputFocusChange Bool
    | InputSelected
    | Resized Viewport
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

        TickedMenu _ ->
            ( { model | menu = Menu.tick model.menu }
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
            ( { model | menu = Menu.setMode mode model.menu }
            , Js.saveState
                { mode = mode
                , speed = Menu.getSpeed model.menu
                }
            )

        SelectedSpeed speed ->
            ( { model | menu = Menu.setSpeed speed model.menu }
            , Js.saveState
                { mode = Menu.getMode model.menu
                , speed = speed
                }
            )

        SelectedRestart ->
            case model.status of
                Playing words _ _ ->
                    ( { model
                        | status = ready words Passed.empty
                        , menu = Menu.toTitle model.menu
                      }
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
                    ( if focused then
                        startPlay newModel

                      else
                        newModel
                    , Cmd.none
                    )

                _ ->
                    ( newModel, Cmd.none )

        InputSelected ->
            let
                newModel =
                    case model.status of
                        Ready _ _ _ ->
                            startPlay model

                        _ ->
                            model
            in
            ( newModel, scrollTop )

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
        Resized { width, height } ->
            ( { model
                | layout = Layout.fromViewport { width = width, height = height }
                , height = height
              }
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
            { model
                | status = Playing words (Passed.pushAnnouncement ann passed) (Game.startGame (Menu.getMode model.menu))
                , menu = Menu.toInGame model.menu
            }

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

                newMenu =
                    if Game.ended game then
                        Menu.toEnded model.menu

                    else
                        model.menu
            in
            { model
                | status = Playing words newPassed newGame
                , randomSeed = newSeed
                , menu = newMenu
            }


pressedEnter : Model -> Model
pressedEnter model =
    case model.status of
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


scrollTop : Cmd Msg
scrollTop =
    Dom.setViewport 0 0
        |> Task.perform (always NoOp)



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
            , Font.color Palette.dark
            , Background.color Palette.light
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
    column [ height (px model.height), width fill ]
        [ bar model.layout AthleteA (Times.get AthleteA times) (Menu.getSpeed model.menu) (isAthlete AthleteA)
        , ticker model
        , bar model.layout AthleteB (Times.get AthleteB times) (Menu.getSpeed model.menu) (isAthlete AthleteB)
        ]



-- Ticker


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
                :: tickerPassed model.inputFocused passed
            )
                |> List.reverse

        athleteM =
            case model.status of
                Playing _ _ game ->
                    Game.getActiveAthlete game
                        |> Maybe.filter (isHumanAthlete (Menu.getMode model.menu))

                _ ->
                    Nothing

        tickerEl act passed =
            el
                [ inFront (inputEl model.layout model.inputFocused athleteM)
                , Font.size (Palette.textSizeLarge model.layout)
                , height (px (fraction 1.2 (Palette.textSizeLarge model.layout)))
                , width fill
                , clip
                ]
                (row
                    ([ alignRight, centerY ]
                        |> consWhen (not model.inputFocused && Maybe.isJust athleteM)
                            (Attribute.blur 5)
                    )
                    (toTickerTexts act passed)
                )

        menuEl =
            menu model.layout model.menu

        tickerMenu act passed =
            let
                wrapper =
                    Keyed.column
                        [ centerY
                        , width fill
                        , Cursor.default
                        ]

                compressedLayout =
                    wrapper
                        [ ( "menu", menuEl )
                        , ( "ticker"
                          , row
                                [ width fill
                                ]
                                [ tickerEl act passed
                                , cursor
                                ]
                          )
                        ]
            in
            case model.layout of
                CompressedSmall ->
                    compressedLayout

                CompressedMedium ->
                    compressedLayout

                _ ->
                    wrapper
                        [ ( "ticker"
                          , row
                                [ width fill
                                , above menuEl
                                ]
                                [ tickerEl act passed
                                , cursor
                                ]
                          )
                        ]
    in
    case model.status of
        Loading ann ->
            tickerMenu (tickerAnnouncement model.inputFocused ann) Passed.empty

        WordsLoadError _ passed ann ->
            tickerMenu (tickerAnnouncement model.inputFocused ann) passed

        Ready _ passed ann ->
            tickerMenu (tickerAnnouncement model.inputFocused ann) passed

        Playing _ passed game ->
            tickerMenu (tickerActive model.inputFocused (Game.getActive game)) passed


inputEl : Layout -> Bool -> Maybe Athlete -> Element Msg
inputEl layout inputFocused athleteM =
    let
        pressHere =
            case athleteM of
                Just athlete ->
                    if not inputFocused then
                        el
                            [ Background.color (athleteColorTransparent athlete)
                            , Font.color Palette.light
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
    el
        [ behindContent pressHere
        , width fill
        , height fill
        , paddingXY 0 (fraction 0.15 (Palette.textSizeLarge layout))
        ]
        (Input.fixedMultiline
            [ Events.onFocus (InputFocusChange True)
            , Events.onLoseFocus (InputFocusChange False)
            , Pointer.onPrimaryDown NoOp InputSelected
            , Font.size 16
            , Font.color Palette.transparent
            , Background.color Palette.transparent
            , width fill
            , height fill
            , padding 0
            , Border.width 0
            , Border.rounded 0
            , focused [ Border.glow Palette.transparent 0 ]
            , Cursor.default
            ]
            { text = ""
            , onChange = Inputted
            , placeholder = Nothing
            , label = Input.labelHidden ""
            , spellcheck = False
            }
        )


tickerActive : Bool -> Maybe Active -> Element Msg
tickerActive inputFocused activeM =
    case activeM of
        Just (Active.AthleteInput athlete input) ->
            el
                [ Font.color (athleteColor athlete)
                , Font.underline
                , Font.bold
                ]
                (text (String.toUpper input))

        Just (Active.Announcement ann) ->
            tickerAnnouncement inputFocused ann

        Nothing ->
            none


tickerAnnouncement : Bool -> Announcement -> Element Msg
tickerAnnouncement inputFocused ann =
    Announcement.getCurrent ann
        |> fromDocParagraph inputFocused


tickerPassed : Bool -> Passed -> List (Element Msg)
tickerPassed inputFocused passed =
    Passed.toList passed
        |> List.map (tickerMessage inputFocused)


tickerMessage : Bool -> Message -> Element Msg
tickerMessage inputFocused tt =
    case tt of
        Message.InterruptedAnnouncement txt ticks ->
            fromDocParagraph inputFocused (Doc.Util.paragraphAppend "—" (Doc.Util.paragraphLeft ticks txt))

        Message.FinishedAnnouncement txt ->
            fromDocParagraph inputFocused txt

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
                    [ Font.color Palette.light
                    , Background.color color
                    ]
                    (text (String.right 1 txtUpper))
                ]

        Message.WrongAthleteInput athlete txt ->
            let
                baseStyle =
                    [ Font.color (athleteColor athlete)
                    , Font.bold
                    ]
            in
            case txt of
                "" ->
                    el baseStyle (text "✖")

                _ ->
                    el
                        ([ Font.strike
                         , Attribute.style "text-decoration-color" (toCssColor Palette.dark)
                         ]
                            ++ baseStyle
                        )
                        (text (String.toUpper txt))



-- Menu


menu : Layout -> Menu -> Element Msg
menu layout menu_ =
    column
        [ alignRight
        , Cursor.default
        , moveDown (0.4 * toFloat (Palette.textSizeLarge layout))
        , spacing (Palette.textLineSpacing (Palette.textSizeNormal layout))
        , Attribute.raise 1
        , paddingEach { sides | right = fraction 0.5 (Palette.textSizeNormal layout) }
        ]
        (Menu.lines menu_
            |> List.padLeft 3 []
            |> List.map (menuLine layout (Menu.getMode menu_))
        )


menuLine : Layout -> GameMode -> MenuLine -> Element Msg
menuLine layout mode line =
    row
        [ alignRight
        , height (px (Palette.textSizeNormal layout))
        ]
        (line
            |> List.map (menuText layout mode)
        )


menuText : Layout -> GameMode -> MenuText -> Element Msg
menuText layout mode mt =
    case mt of
        MenuText.PlainText txt opts ->
            el (menuTextStyle mode opts)
                (text txt)

        MenuText.PressableText txt action opts ->
            case action of
                MenuAction.AuthorLink ->
                    newTabLink (menuTextStyle mode opts) { label = text txt, url = "http://agj.cl" }

                MenuAction.ChangeGameMode newMode ->
                    el
                        (menuTextStyle mode opts
                            ++ [ Pointer.onPrimaryDown NoOp (SelectedMode newMode)
                               , Cursor.pointer
                               ]
                        )
                        (text txt)

                MenuAction.ChangeSpeed speed ->
                    el
                        (menuTextStyle mode opts
                            ++ [ Pointer.onPrimaryDown NoOp (SelectedSpeed speed)
                               , Cursor.pointer
                               ]
                        )
                        (text txt)

                MenuAction.Restart ->
                    el
                        (menuTextStyle mode opts
                            ++ [ Pointer.onPrimaryDown NoOp SelectedRestart
                               , Cursor.pointer
                               ]
                        )
                        (text txt)


menuTextStyle : GameMode -> MenuTextOptions -> List (Element.Attribute Msg)
menuTextStyle mode { bold, color } =
    [ Font.color
        (case color of
            MenuText.Dark ->
                Palette.dark

            MenuText.Gray ->
                Palette.darkish

            MenuText.Highlit ->
                case mode of
                    HotseatMode ->
                        Palette.athleteA

                    SingleMode ->
                        Palette.athleteB
        )
    ]
        |> consWhen bold Font.bold



-- Time bar


bar : Layout -> Athlete -> Float -> Speed -> Bool -> Element Msg
bar layout athlete timeLeft speed active =
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
            , Transition.each
                [ Transition.property "height" 150 [ Transition.easeOutQuad ]
                ]
            ]
            [ el
                [ width (fillPortion emptyPortion)
                , height fill
                , Background.color (athleteColorDark athlete)
                , Transition.each
                    [ Transition.property "flex-grow" (Levers.tickInterval speed |> round) [ Transition.linear ] ]
                ]
                none
            , el
                [ width (fillPortion filledPortion)
                , height fill
                , Background.color (athleteColor athlete)
                , Transition.each
                    [ Transition.property "flex-grow" (Levers.tickInterval speed |> round) [ Transition.linear ] ]
                ]
                none
            ]
        )



-- Other view


fromDocParagraph : Bool -> Paragraph.Paragraph -> Element msg
fromDocParagraph inputFocused par =
    row [] <|
        List.map (fromDocText inputFocused) (Paragraph.content par)


fromDocText : Bool -> Doc.Text.Text -> Element msg
fromDocText inputFocused txt =
    let
        textContent =
            Doc.Text.content txt
                |> String.toUpper

        style =
            Doc.Text.format txt
                |> getStyle inputFocused
    in
    el style (text textContent)


getStyle : Bool -> Doc.Format.Format -> List (Element.Attribute msg)
getStyle inputFocused format =
    let
        color =
            case Doc.Format.athlete format of
                Just athlete ->
                    athleteColor athlete

                Nothing ->
                    if inputFocused then
                        Palette.dark

                    else
                        Palette.darkish

        colors =
            if Doc.Format.isInverted format then
                [ Font.color Palette.light
                , Background.color color
                ]

            else
                [ Font.color color ]
    in
    colors
        |> consWhen (Doc.Format.isBold format)
            Font.bold
        |> consWhen (Doc.Format.isItalic format)
            Font.italic



-- |> consWhen (not inputFocused)
--     (Attribute.blur 10)


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
        ([ Browser.Events.onResize (\w h -> Resized (Viewport w h))
         , Time.every (Levers.tickInterval (Menu.getSpeed model.menu)) Ticked
         ]
            |> consWhen (Menu.animating model.menu)
                (Time.every Levers.menuTickInterval TickedMenu)
        )



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
