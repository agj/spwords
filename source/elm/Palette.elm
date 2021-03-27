module Palette exposing (..)

import Element exposing (..)
import Element.Font as Font exposing (Font)



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


textSizeSmall : Int
textSizeSmall =
    13


textSizeNormal : Int
textSizeNormal =
    16


textSizeLarge : Int
textSizeLarge =
    26


textSizeLarger : Int
textSizeLarger =
    100


textLineSpacing : Int -> Int
textLineSpacing fontSize =
    round (toFloat fontSize * 0.4)



-- SPACING


spaceLarge : Int
spaceLarge =
    26


spaceNormal : Int
spaceNormal =
    20


spaceSmall : Int
spaceSmall =
    10


spaceSmaller : Int
spaceSmaller =
    5


spaceSmallest : Int
spaceSmallest =
    2
