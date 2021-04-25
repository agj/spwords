module Menu.MenuText exposing
    ( MenuAction(..)
    , MenuText(..)
    , MenuTextColor(..)
    , MenuTextOptions
    , dropLeft
    , dropRight
    , left
    , length
    , plainText
    , pressableText
    , right
    , setBold
    , setColor
    , toString
    )

import Game.GameMode exposing (GameMode)
import Speed exposing (Speed)


type MenuText
    = PlainText String MenuTextOptions
    | PressableText String MenuAction MenuTextOptions


type alias MenuTextOptions =
    { bold : Bool
    , color : MenuTextColor
    }


type MenuTextColor
    = Dark
    | Gray
    | Highlit


type MenuAction
    = AuthorLink
    | ChangeGameMode GameMode
    | ChangeSpeed Speed
    | Restart


plainText : String -> MenuText
plainText str =
    PlainText str standardOptions


pressableText : String -> MenuAction -> MenuText
pressableText str action =
    PressableText str action standardOptions


length : MenuText -> Int
length mt =
    toString mt |> String.length


setBold : Bool -> MenuText -> MenuText
setBold value mt =
    mapOptions (\opt -> { opt | bold = value }) mt


setColor : MenuTextColor -> MenuText -> MenuText
setColor value mt =
    mapOptions (\opt -> { opt | color = value }) mt


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


dropLeft : Int -> MenuText -> MenuText
dropLeft len mt =
    mapText (String.dropLeft len) mt


dropRight : Int -> MenuText -> MenuText
dropRight len mt =
    mapText (String.dropRight len) mt


mapText : (String -> String) -> MenuText -> MenuText
mapText fn mt =
    case mt of
        PlainText str opts ->
            PlainText (fn str) opts

        PressableText str action opts ->
            PressableText (fn str) action opts


mapOptions : (MenuTextOptions -> MenuTextOptions) -> MenuText -> MenuText
mapOptions fn mt =
    case mt of
        PlainText str opts ->
            PlainText str (fn opts)

        PressableText str action opts ->
            PressableText str action (fn opts)



-- INTERNAL


standardOptions : MenuTextOptions
standardOptions =
    { bold = False
    , color = Gray
    }
