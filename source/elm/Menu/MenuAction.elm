module Menu.MenuAction exposing (MenuAction(..))

import Game.GameMode exposing (GameMode)
import Speed exposing (Speed)


type MenuAction
    = AuthorLink
    | ChangeGameMode GameMode
    | ChangeSpeed Speed
    | Restart
