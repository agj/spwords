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
    { viewport : Viewport
    }


type alias Flags =
    { viewport : { width : Int, height : Int }
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { viewport = flags.viewport
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Resized
    | GotViewport Viewport
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        modelMsg =
            ( model, Cmd.none )
    in
    case msg of
        Resized ->
            modelMsg
                |> R.command Viewport.get

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
            mainScreen
        ]
    }


mainScreen : Element Msg
mainScreen =
    column [ height fill, width fill ]
        [ bar AthleteA
        , statusDisplay
        , bar AthleteB
        ]


type Athlete
    = AthleteA
    | AthleteB


bar : Athlete -> Element Msg
bar athlete =
    el
        [ width fill
        , height (px Palette.spaceSmall)
        , Background.color
            (case athlete of
                AthleteA ->
                    Palette.athleteA

                AthleteB ->
                    Palette.athleteB
            )
        ]
        none


statusDisplay : Element Msg
statusDisplay =
    el
        [ clip
        , width fill
        , centerY
        , Font.size Palette.textSizeLarger
        ]
        (el [ alignRight ] <| text "Welcome to Spwords!")



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize <|
            \w h -> Resized
        , Viewport.got GotViewport NoOp
        ]
