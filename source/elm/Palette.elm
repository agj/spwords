module Palette exposing (..)

import Element exposing (..)
import Element.Font as Font exposing (Font)
import Layout exposing (Layout)



-- COLOR


light : Color
light =
    rgb 0.9 0.9 0.9


dark : Color
dark =
    rgb 0.2 0.2 0.2


athleteA : Color
athleteA =
    rgb 1 0 1


athleteADark : Color
athleteADark =
    rgb 0.3 0 0.3


athleteB : Color
athleteB =
    rgb 0 1 1


athleteBDark : Color
athleteBDark =
    rgb 0 0.3 0.3


transparent : Color
transparent =
    rgba 0 0 0 0



-- TEXT


font : List Font
font =
    [ Font.typeface "Source Code Pro"
    , Font.monospace
    ]


textSizeNormal : Layout -> Int
textSizeNormal layout =
    case layout of
        Layout.Small ->
            17

        Layout.Medium ->
            23

        Layout.Large ->
            30


textSizeLarge : Layout -> Int
textSizeLarge layout =
    case layout of
        Layout.Small ->
            55

        Layout.Medium ->
            80

        Layout.Large ->
            110


textLineSpacing : Int -> Int
textLineSpacing fontSize =
    round (toFloat fontSize * 0.2)



-- SPACING


spaceLarge : Layout -> Int
spaceLarge layout =
    case layout of
        Layout.Small ->
            18

        _ ->
            30


spaceNormal : Layout -> Int
spaceNormal layout =
    case layout of
        Layout.Small ->
            6

        _ ->
            10


spaceSmall : Layout -> Int
spaceSmall layout =
    case layout of
        Layout.Small ->
            3

        _ ->
            5
