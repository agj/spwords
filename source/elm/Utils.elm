module Utils exposing (..)

import Athlete exposing (..)
import Element
import Random


fraction : Float -> Int -> Int
fraction frac num =
    round (frac * toFloat num)


ifElse : Bool -> a -> a -> a
ifElse check yes no =
    if check then
        yes

    else
        no


unnest : List (List a) -> List a
unnest list =
    List.concatMap identity list


sides =
    { left = 0
    , right = 0
    , top = 0
    , bottom = 0
    }


toCssColor : Element.Color -> String
toCssColor color =
    let
        { red, green, blue, alpha } =
            Element.toRgb color
    in
    "rgba("
        ++ String.fromInt (round <| red * 255)
        ++ ", "
        ++ String.fromInt (round <| green * 255)
        ++ ", "
        ++ String.fromInt (round <| blue * 255)
        ++ ", "
        ++ String.fromFloat (alpha * 100)
        ++ "%"
        ++ ")"


stringHead : String -> Maybe Char
stringHead str =
    String.uncons str
        |> Maybe.map Tuple.first


stringLast : String -> Maybe Char
stringLast str =
    stringCharAt (String.length str - 1) str


stringCharAt : Int -> String -> Maybe Char
stringCharAt index str =
    str
        |> String.dropLeft index
        |> stringHead


stringMember : Char -> String -> Bool
stringMember char str =
    String.any (\ch -> char == ch) str


oppositeAthlete : Athlete -> Athlete
oppositeAthlete athlete =
    case athlete of
        AthleteA ->
            AthleteB

        AthleteB ->
            AthleteA



-- RANDOM


randomItem : Random.Seed -> List a -> ( Maybe a, Random.Seed )
randomItem seed list =
    Random.step (itemGenerator list) seed


itemGenerator : List a -> Random.Generator (Maybe a)
itemGenerator list =
    Random.int 0 (List.length list - 1)
        |> Random.map (indexToItem list)


indexToItem : List a -> Int -> Maybe a
indexToItem list index =
    list
        |> List.drop index
        |> List.head
