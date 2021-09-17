module Menu exposing
    ( Menu
    , animating
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
import Levers exposing (Ticks)
import List.Extra as List
import Menu.MenuAction as MenuAction exposing (MenuAction)
import Menu.MenuLine as MenuLine exposing (MenuLine)
import Menu.MenuText as MenuText exposing (MenuText(..), MenuTextOptions)
import Speed exposing (Speed)
import Util.List as List


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
lines ((Menu _ { transition }) as menu) =
    let
        curLines =
            currentLines menu
    in
    case transition of
        Stable ->
            curLines

        Transitioning t oldLines ->
            transitionLines t oldLines curLines


animating : Menu -> Bool
animating (Menu _ { transition }) =
    transition /= Stable



-- SETTERS


tick : Menu -> Menu
tick ((Menu state data) as menu) =
    case data.transition of
        Stable ->
            menu

        Transitioning t oldLines ->
            let
                newTicks =
                    t + 1

                newTransition =
                    if transitionDone newTicks oldLines (currentLines menu) then
                        Stable

                    else
                        Transitioning newTicks oldLines
            in
            Menu state { data | transition = newTransition }


setMode : GameMode -> Menu -> Menu
setMode mode ((Menu state data) as menu) =
    if data.mode == mode then
        menu

    else
        Menu state
            { data
                | mode = mode
                , transition = startTransition menu
            }


setSpeed : Speed -> Menu -> Menu
setSpeed speed ((Menu state data) as menu) =
    if data.speed == speed then
        menu

    else
        Menu state
            { data
                | speed = speed
                , transition = startTransition menu
            }


toTitle : Menu -> Menu
toTitle menu =
    toState Title menu


toInGame : Menu -> Menu
toInGame menu =
    toState InGame menu


toEnded : Menu -> Menu
toEnded menu =
    toState Ended menu



---------------------- INTERNAL


toState : MenuState -> Menu -> Menu
toState targetState ((Menu currentState data) as menu) =
    if currentState == targetState then
        menu

    else
        Menu targetState { data | transition = startTransition menu }


startTransition : Menu -> Transition
startTransition menu =
    Transitioning 0 (makeOldLines menu)


makeOldLines : Menu -> List MenuLine
makeOldLines menu =
    lines menu
        |> List.map
            (\menuLine ->
                menuLine
                    |> List.map
                        (\menuText ->
                            case menuText of
                                PlainText str opts ->
                                    PlainText str
                                        { opts
                                            | color = MenuText.Gray
                                        }

                                PressableText str _ opts ->
                                    PlainText str { opts | color = MenuText.Gray }
                        )
            )


transitionDone : Int -> List MenuLine -> List MenuLine -> Bool
transitionDone ticks oldLines newLines =
    allLinesDone ticks newLines && allLinesDone ticks oldLines


allLinesDone : Int -> List MenuLine -> Bool
allLinesDone ticks ls =
    ls
        |> List.map MenuLine.length
        |> List.all
            (\lineLength ->
                ticks - (Levers.menuTransitionTrail + 2 * Levers.menuTransitionInclination) >= lineLength
            )


currentLines : Menu -> List MenuLine
currentLines (Menu state { mode, speed }) =
    case state of
        Title ->
            titleLines mode speed

        InGame ->
            inGameLines

        Ended ->
            endedLines


transitionLines : Int -> List MenuLine -> List MenuLine -> List MenuLine
transitionLines t from to =
    let
        take =
            Levers.menuTransitionTrail

        transformer pos ml =
            let
                highlit =
                    MenuLine.dropRight (pos - take) ml
                        |> List.map (MenuText.setColor MenuText.Highlit)

                rest =
                    MenuLine.right (pos - take) ml
            in
            highlit ++ rest
    in
    stateLines transformer t from to


stateLines : (Int -> MenuLine -> MenuLine) -> Int -> List MenuLine -> List MenuLine -> List MenuLine
stateLines rightTransformer t from to =
    let
        maxHeight =
            max (List.length from) (List.length to)

        indices =
            List.range 0 (maxHeight - 1)
                |> List.reverse

        left =
            from
                |> List.padLeft maxHeight []
                |> List.zip indices
                |> List.map processLeft

        processLeft ( i, ml ) =
            MenuLine.dropRight
                (t - i * Levers.menuTransitionInclination)
                ml

        right =
            to
                |> List.padLeft maxHeight []
                |> List.zip indices
                |> List.map processRight

        processRight ( i, ml ) =
            let
                take =
                    t - i * Levers.menuTransitionInclination
            in
            ml
                |> MenuLine.right take
                |> MenuLine.padLeft take ' '
                |> rightTransformer take

        join ( l, r ) =
            l ++ r
    in
    List.zip left right
        |> List.map join



-- LINES


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
    , [ MenuText.pressable ("[" ++ modeName ++ "]") (MenuAction.ChangeGameMode nextMode)
      , MenuText.plain " MODE."
      ]
    , [ MenuText.pressable ("[" ++ speedName ++ "]") (MenuAction.ChangeSpeed nextSpeed)
      , MenuText.plain " SPEED."
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
    [ MenuText.plain "SPWORDS" |> MenuText.setColor MenuText.Dark |> MenuText.setBold True
    , MenuText.plain " BY " |> MenuText.setColor MenuText.Dark
    , MenuText.pressable "AGJ." MenuAction.AuthorLink |> MenuText.setColor MenuText.Dark
    ]


restartLine : MenuLine
restartLine =
    [ MenuText.pressable "[RESTART]" MenuAction.Restart
    ]
