module Menu exposing (Menu, MenuAction(..), MenuLine, MenuText(..), MenuTextOptions, lines, start)

import Game.GameMode exposing (GameMode(..))
import Speed exposing (Speed)


type
    Menu
    -- = Transitioning Float (List MenuLine) (List MenuLine)
    = Stable (List MenuLine)


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


start : Menu
start =
    title HotseatMode Speed.Normal


lines : Menu -> List MenuLine
lines menu =
    case menu of
        Stable ls ->
            ls



-- setMode : GameMode -> Menu -> Menu
-- setMode mode menu =
--     case menu of
--         Stable lines ->
--             Stable title
-- INTERNAL


title : GameMode -> Speed -> Menu
title mode speed =
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
    Stable
        [ [ PlainText "SPWORDS" boldDark
          , PlainText " BY " boldDark
          , PressableText "AGJ. " AuthorLink boldDark
          ]
        , [ PressableText ("[" ++ modeName ++ "]") (ChangeGameMode nextMode) normal
          , PlainText " MODE. " normal
          ]
        , [ PressableText ("[" ++ speedName ++ "]") (ChangeSpeed nextSpeed) normal
          , PlainText " SPEED. " normal
          ]
        ]


normal : MenuTextOptions
normal =
    { bold = False, dark = False }


boldDark : MenuTextOptions
boldDark =
    { bold = True, dark = True }
