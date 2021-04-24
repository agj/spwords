module Menu.MenuText exposing (..)

import Game.GameMode exposing (GameMode)
import Speed exposing (Speed)


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


length : MenuText -> Int
length mt =
    toString mt |> String.length


toString : MenuText -> String
toString mt =
    case mt of
        PlainText str _ ->
            str

        PressableText str _ _ ->
            str


left : Int -> MenuText -> MenuText
left len mt =
    mapText (String.left len) mt


right : Int -> MenuText -> MenuText
right len mt =
    mapText (String.right len) mt


mapText : (String -> String) -> MenuText -> MenuText
mapText fn mt =
    case mt of
        PlainText str opts ->
            PlainText (fn str) opts

        PressableText str action opts ->
            PressableText (fn str) action opts
