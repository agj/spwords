module Util exposing (..)


fraction : Float -> Int -> Int
fraction frac num =
    round (frac * toFloat num)


ifElse : Bool -> a -> a -> a
ifElse check yes no =
    if check then
        yes

    else
        no
