module Util.Random exposing (..)

import Random


item : Random.Seed -> List a -> ( Maybe a, Random.Seed )
item seed list =
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
