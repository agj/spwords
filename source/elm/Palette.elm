module Palette exposing (..)

import Element exposing (..)
import Element.Font as Font exposing (Font)



-- COLOR


highlight : Color
highlight =
    rgb 0.95 0.6 0.24


warning : Color
warning =
    rgb 0.95 0.24 0.24


light : Color
light =
    rgb 1 1 1


dark : Color
dark =
    rgb 0 0 0


darkish : Color
darkish =
    rgb 0.4 0.4 0.4


mid : Color
mid =
    rgb 0.7 0.7 0.7


backA : Color
backA =
    rgb 0.9 0.9 0.9


backALight : Color
backALight =
    rgb 0.95 0.95 0.95


darkishTransparent : Color
darkishTransparent =
    rgba 0.4 0.4 0.4 0.7


transparent : Color
transparent =
    rgba 0 0 0 0



-- TEXT


font : List Font
font =
    [ Font.sansSerif
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
    36


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
