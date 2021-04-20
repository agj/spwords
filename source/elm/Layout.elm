module Layout exposing (..)

import Viewport exposing (Viewport)


type Layout
    = Large
    | Medium
    | Small


fromViewport : Viewport -> Layout
fromViewport vp =
    if vp.width <= 450 then
        Small

    else if vp.width <= 850 then
        Medium

    else
        Large
