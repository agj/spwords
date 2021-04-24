module Menu exposing
    ( Menu
    , MenuAction(..)
    , MenuLine
    , MenuText(..)
    , MenuTextOptions
    , lines
    , start
    , toEnded
    , toInGame
    , toTitle
    )

import Game.GameMode exposing (GameMode(..))
import Speed exposing (Speed)


type Menu
    = Title GameMode Speed Transition
    | InGame Transition
    | Ended Transition


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
    | Transitioning Float (List MenuLine)


start : Menu
start =
    Title SingleMode Speed.Normal Stable


toTitle : GameMode -> Speed -> Menu -> Menu
toTitle mode speed menu =
    Title mode speed Stable


toInGame : Menu -> Menu
toInGame menu =
    InGame Stable


toEnded : Menu -> Menu
toEnded menu =
    Ended Stable


lines : Menu -> List MenuLine
lines menu =
    case menu of
        Title mode speed _ ->
            titleLines mode speed

        InGame _ ->
            inGameLines

        Ended _ ->
            endedLines



-- INTERNAL


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
    , PlainText " BY " boldDark
    , PressableText "AGJ. " AuthorLink boldDark
    ]


restartLine : MenuLine
restartLine =
    [ PressableText "[RESTART]" Restart normal
    ]


normal : MenuTextOptions
normal =
    { bold = False, dark = False }


boldDark : MenuTextOptions
boldDark =
    { bold = True, dark = True }
