module Utils exposing (..)

import Element exposing (Color)
import Palette


fraction : Float -> Int -> Int
fraction frac num =
    round (frac * toFloat num)


ifElse : Bool -> a -> a -> a
ifElse check yes no =
    if check then
        yes

    else
        no


unnest : List (List a) -> List a
unnest list =
    List.concatMap identity list


sides =
    { left = 0
    , right = 0
    , top = 0
    , bottom = 0
    }


toCssColor : Element.Color -> String
toCssColor color =
    let
        { red, green, blue, alpha } =
            Element.toRgb color
    in
    "rgba("
        ++ String.fromInt (round <| red * 255)
        ++ ", "
        ++ String.fromInt (round <| green * 255)
        ++ ", "
        ++ String.fromInt (round <| blue * 255)
        ++ ", "
        ++ String.fromFloat (alpha * 100)
        ++ "%"
        ++ ")"


intersperse : a -> List a -> List a
intersperse thing list =
    list
        |> List.concatMap (\v -> [ thing, v ])
        |> List.tail
        |> Maybe.withDefault []
