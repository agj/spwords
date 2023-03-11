module Palette exposing
    ( athleteA
    , athleteADark
    , athleteATransparent
    , athleteB
    , athleteBDark
    , athleteBTransparent
    , dark
    , darkish
    , font
    , light
    , spaceLarge
    , spaceNormal
    , spaceSmall
    , textLineSpacing
    , textSizeLarge
    , textSizeNormal
    , transparent
    )

import Color exposing (Color)
import Color.Manipulate exposing (darken, desaturate, fadeOut, lighten)
import Element
import Element.Font as Font exposing (Font)
import Layout exposing (Layout)
import Util exposing (fraction)



-- COLOR


dark : Element.Color
dark =
    toElmUi dark_


darkish : Element.Color
darkish =
    toElmUi darkish_


light : Element.Color
light =
    toElmUi light_


athleteA : Element.Color
athleteA =
    toElmUi athleteA_


athleteADark : Element.Color
athleteADark =
    toElmUi athleteADark_


athleteATransparent : Element.Color
athleteATransparent =
    toElmUi athleteATransparent_


athleteB : Element.Color
athleteB =
    toElmUi athleteB_


athleteBDark : Element.Color
athleteBDark =
    toElmUi athleteBDark_


athleteBTransparent : Element.Color
athleteBTransparent =
    toElmUi athleteBTransparent_


transparent : Element.Color
transparent =
    Element.rgba 0 0 0 0



-- TEXT


font : List Font
font =
    [ Font.typeface "Source Code Pro"
    , Font.monospace
    ]


textSizeNormal : Layout -> Int
textSizeNormal =
    scalable 0.6 30


textSizeLarge : Layout -> Int
textSizeLarge =
    scalable 1 110


textLineSpacing : Int -> Int
textLineSpacing fontSize =
    fraction 0.2 fontSize



-- SPACING


spaceLarge : Layout -> Int
spaceLarge =
    scalable 1 30


spaceNormal : Layout -> Int
spaceNormal =
    scalable 1 10


spaceSmall : Layout -> Int
spaceSmall =
    scalable 1 5



-- INTERNAL COLORS


dark_ =
    Color.hsl 0 0 0


light_ =
    Color.hsl (deg 46.7) (pc 32.5) (pc 95.9)


darkish_ =
    light_
        |> darken 0.4
        |> desaturate 0.7


athleteA_ =
    Color.hsl (deg 313.3) (pc 83) (pc 48.4)


athleteADark_ =
    athleteA_
        |> lighten 0.4


athleteATransparent_ =
    athleteA_
        |> fadeOut 0.1


athleteB_ =
    Color.hsl (deg 200.8) (pc 84.7) (pc 50.9)


athleteBDark_ =
    athleteB_
        |> lighten 0.4


athleteBTransparent_ =
    athleteB_
        |> fadeOut 0.1



-- INTERNAL UTILS


scalable : Float -> Int -> Layout -> Int
scalable range largest layout =
    let
        d n =
            range * n + (1 - range)
    in
    case layout of
        Layout.Large ->
            largest

        Layout.Medium ->
            fraction (d 0.73) largest

        Layout.Small ->
            fraction (d 0.5) largest

        Layout.CompressedMedium ->
            fraction (d 0.5) largest

        Layout.CompressedSmall ->
            fraction (d 0.3) largest


toElmUi : Color -> Element.Color
toElmUi color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    Element.rgba red green blue alpha


deg : Float -> Float
deg n =
    n / 360


pc : Float -> Float
pc n =
    n / 100
