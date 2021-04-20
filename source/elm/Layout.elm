module Layout exposing (..)

import Viewport exposing (Viewport)


type Layout
    = Large
    | Medium
    | Small


fromViewport : Viewport -> Layout
fromViewport vp =
    if vp.width <= 400 then
        Small

    else
        Medium
