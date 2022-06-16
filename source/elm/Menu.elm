module Menu exposing
    ( Menu
    , MenuState(..)
    , animating
    , lines
    , start
    , tick
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
    = Menu MenuData


type MenuState
    = Title
    | InGame
    | Ended


type alias MenuData =
    { transition : Transition
    , state : MenuState
    , mode : GameMode
    , speed : Speed
    }


type Transition
    = Stable
    | Transitioning Int (List MenuLine)


start : MenuState -> GameMode -> Speed -> Menu
start state mode speed =
    Menu
        { transition = Stable
        , state = state
        , mode = mode
        , speed = speed
        }



-- ACCESSORS


lines : Menu -> List MenuLine
lines ((Menu { transition }) as menu) =
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
animating (Menu { transition }) =
    transition /= Stable



-- SETTERS


tick : MenuState -> GameMode -> Speed -> Menu -> Menu
tick state mode speed ((Menu data) as menu) =
    if state /= data.state || mode /= data.mode || speed /= data.speed then
        Menu
            { data
                | mode = mode
                , speed = speed
                , state = state
                , transition = startTransition menu
            }

    else
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
                Menu { data | transition = newTransition }



---------------------- INTERNAL


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
currentLines (Menu { mode, speed, state }) =
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
