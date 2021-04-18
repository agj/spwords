module Util.Element exposing (..)

import Element


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
