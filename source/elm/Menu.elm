module Menu exposing
    ( Menu
    , getMode
    , getSpeed
    , lines
    , setMode
    , setSpeed
    , start
    , tick
    , toEnded
    , toInGame
    , toTitle
    )

import Game.GameMode exposing (GameMode(..))
import List.Extra
import Menu.MenuLine as MenuLine exposing (MenuLine)
import Menu.MenuText as MenuText exposing (MenuAction(..), MenuText(..), MenuTextOptions)
import Speed exposing (Speed)


type Menu
    = Menu MenuState MenuData


type MenuState
    = Title
    | InGame
    | Ended


type alias MenuData =
    { mode : GameMode
    , speed : Speed
    , transition : Transition
    }


type Transition
    = Stable
    | Transitioning Int (List MenuLine)


start : Menu
start =
    Menu Title
        { mode = SingleMode
        , speed = Speed.Normal
        , transition = Stable
        }



-- ACCESSORS


getMode : Menu -> GameMode
getMode (Menu _ { mode }) =
    mode


getSpeed : Menu -> Speed
getSpeed (Menu _ { speed }) =
    speed


lines : Menu -> List MenuLine
lines (Menu state { mode, speed, transition }) =
    let
        curLines =
            case state of
                Title ->
                    titleLines mode speed

                InGame ->
                    inGameLines

                Ended ->
                    endedLines
    in
    case transition of
        Stable ->
            curLines

        Transitioning t oldLines ->
            transitionLines t oldLines curLines



-- SETTERS


tick : Menu -> Menu
tick ((Menu state data) as menu) =
    case data.transition of
        Stable ->
            menu

        Transitioning t ls ->
            let
                newTicks =
                    t + 1

                newTransition =
                    if transitionDone newTicks ls (lines menu) then
                        Stable

                    else
                        Transitioning newTicks ls
            in
            Menu state { data | transition = newTransition }


setMode : GameMode -> Menu -> Menu
setMode mode (Menu state data) =
    Menu state { data | mode = mode }


setSpeed : Speed -> Menu -> Menu
setSpeed speed (Menu state data) =
    Menu state { data | speed = speed }


toTitle : Menu -> Menu
toTitle menu =
    toState Title menu


toInGame : Menu -> Menu
toInGame menu =
    toState InGame menu


toEnded : Menu -> Menu
toEnded menu =
    toState Ended menu



-- INTERNAL


toState : MenuState -> Menu -> Menu
toState targetState ((Menu currentState data) as menu) =
    if currentState == targetState then
        menu

    else
        Menu targetState { data | transition = Transitioning 0 (lines menu) }


transitionDone : Int -> List MenuLine -> List MenuLine -> Bool
transitionDone ticks oldLines newLines =
    allLinesDone ticks oldLines && allLinesDone ticks newLines


allLinesDone : Int -> List MenuLine -> Bool
allLinesDone ticks ls =
    ls
        |> List.map MenuLine.length
        |> List.all (\l -> l >= ticks)


transitionLines : Int -> List MenuLine -> List MenuLine -> List MenuLine
transitionLines t from to =
    let
        maxHeight =
            max (List.length from) (List.length to)

        left =
            elongate maxHeight from
                |> List.map (MenuLine.dropRight t)

        right =
            elongate maxHeight to
                |> List.map (MenuLine.right t)

        join ( l, r ) =
            l ++ r
    in
    List.Extra.zip left right
        |> List.map join


elongate : Int -> List MenuLine -> List MenuLine
elongate len ls =
    List.repeat (len - List.length ls) [] ++ ls



--


titleLines : GameMode -> Speed -> List MenuLine
titleLines mode speed =
    let
        modeName =
            case mode of
                HotseatMode ->
                    "2P HOTSEAT"

                SingleMode ->
                    "SOLO"

        speedName =
            case speed of
                Speed.Normal ->
                    "TOURNAMENT"

                Speed.Slow ->
                    "AMATEUR"

        nextMode =
            case mode of
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
    in
    [ titleLine
    , [ PressableText ("[" ++ modeName ++ "]") (ChangeGameMode nextMode) normal
      , PlainText " MODE. " normal
      ]
    , [ PressableText ("[" ++ speedName ++ "]") (ChangeSpeed nextSpeed) normal
      , PlainText " SPEED. " normal
      ]
    ]


inGameLines : List MenuLine
inGameLines =
    [ restartLine
    ]


endedLines : List MenuLine
endedLines =
    [ titleLine
    , restartLine
    ]


titleLine : MenuLine
titleLine =
    [ PlainText "SPWORDS" boldDark
    , PlainText " BY " dark
    , PressableText "AGJ. " AuthorLink dark
    ]


restartLine : MenuLine
restartLine =
    [ PressableText "[RESTART] " Restart normal
    ]


normal : MenuTextOptions
normal =
    { bold = False, dark = False }


boldDark : MenuTextOptions
boldDark =
    { bold = True, dark = True }


dark : MenuTextOptions
dark =
    { bold = False, dark = True }
