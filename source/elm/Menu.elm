module Menu exposing
    ( Menu
    , MenuAction(..)
    , MenuLine
    , MenuText(..)
    , MenuTextOptions
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


type alias MenuLine =
    List MenuText


type MenuText
    = PlainText String MenuTextOptions
    | PressableText String MenuAction MenuTextOptions


type alias MenuTextOptions =
    { bold : Bool, dark : Bool }


type MenuAction
    = AuthorLink
    | ChangeGameMode GameMode
    | ChangeSpeed Speed
    | Restart


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
lines (Menu state { mode, speed }) =
    case state of
        Title ->
            titleLines mode speed

        InGame ->
            inGameLines

        Ended ->
            endedLines



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
toTitle ((Menu state data) as menu) =
    if state == Title then
        menu

    else
        Menu Title data


toInGame : Menu -> Menu
toInGame ((Menu state data) as menu) =
    if state == InGame then
        menu

    else
        Menu InGame data


toEnded : Menu -> Menu
toEnded ((Menu state data) as menu) =
    if state == Ended then
        menu

    else
        Menu Ended data



-- INTERNAL


transitionDone : Int -> List MenuLine -> List MenuLine -> Bool
transitionDone ticks oldLines newLines =
    allLinesDone ticks oldLines && allLinesDone ticks newLines


allLinesDone : Int -> List MenuLine -> Bool
allLinesDone ticks ls =
    ls
        |> List.map lineLength
        |> List.all ((>=) ticks)


lineLength : MenuLine -> Int
lineLength line =
    line
        |> List.map (getText >> String.length)
        |> List.foldl (+) 0


getText : MenuText -> String
getText mt =
    case mt of
        PlainText str _ ->
            str

        PressableText str _ _ ->
            str


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
