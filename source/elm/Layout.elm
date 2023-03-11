module Layout exposing (..)

import Viewport exposing (Viewport)


type Layout
    = Large
    | Medium
    | Small
    | CompressedMedium
    | CompressedSmall


fromViewport : Viewport -> Layout
fromViewport vp =
    if vp.height <= 220 then
        CompressedSmall

    else if vp.height <= 380 then
        CompressedMedium

    else if vp.width <= 450 then
        Small

    else if vp.width <= 850 then
        Medium

    else
        Large
