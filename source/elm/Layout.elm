module Layout exposing (..)

import Viewport exposing (Viewport)


type Layout
    = Large
    | Medium
    | Small
    | Smaller


fromViewport : Viewport -> Layout
fromViewport vp =
    if vp.height <= 190 then
        Smaller

    else if vp.width <= 450 || vp.height <= 350 then
        Small

    else if vp.width <= 850 then
        Medium

    else
        Large
