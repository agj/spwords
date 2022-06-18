module Util.Element.Color exposing (..)

import Element exposing (Color)


toCssString : Color -> String
toCssString color =
    let
        c =
            Debug.log "color"
                (Element.toRgb color)
    in
    "rgba("
        ++ String.fromFloat (c.red * 0xFF)
        ++ ", "
        ++ String.fromFloat (c.green * 0xFF)
        ++ ", "
        ++ String.fromFloat (c.blue * 0xFF)
        ++ ", "
        ++ String.fromFloat (c.alpha * 0xFF)
        ++ ")"
