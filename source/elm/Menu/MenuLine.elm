module Menu.MenuLine exposing (..)

import Menu.MenuText as MenuText exposing (MenuText(..))


type alias MenuLine =
    List MenuText


length : MenuLine -> Int
length line =
    line
        |> List.map MenuText.length
        |> List.foldl (+) 0


left : Int -> MenuLine -> MenuLine
left len ml =
    let
        process : MenuText -> MenuLine -> MenuLine
        process mt res =
            let
                lenSoFar =
                    length res
            in
            if lenSoFar >= len then
                res

            else
                res ++ [ MenuText.left (len - lenSoFar) mt ]
    in
    List.foldl process [] ml


right : Int -> MenuLine -> MenuLine
right len ml =
    let
        process : MenuText -> MenuLine -> MenuLine
        process mt res =
            let
                lenSoFar =
                    length res
            in
            if lenSoFar >= len then
                res

            else
                MenuText.right (len - lenSoFar) mt :: res
    in
    List.foldr process [] ml


dropRight : Int -> MenuLine -> MenuLine
dropRight len ml =
    left (length ml - len) ml


padLeft : Int -> Char -> MenuLine -> MenuLine
padLeft len ch ml =
    let
        padding =
            String.repeat (len - length ml) (String.fromChar ch)
    in
    if String.length padding > 0 then
        PlainText padding { bold = False, dark = False } :: ml

    else
        ml
