module Util.Random exposing (..)

import Random exposing (Generator, Seed)


item : Seed -> List a -> ( Maybe a, Seed )
item seed list =
    Random.step (itemGenerator list) seed


itemGenerator : List a -> Generator (Maybe a)
itemGenerator list =
    Random.int 0 (List.length list - 1)
        |> Random.map (indexToItem list)


itemGeneratorWithDefault : a -> List a -> Generator a
itemGeneratorWithDefault default list =
    Random.int 0 (List.length list - 1)
        |> Random.map (indexToItem list)
        |> Random.map (Maybe.withDefault default)


indexToItem : List a -> Int -> Maybe a
indexToItem list index =
    list
        |> List.drop index
        |> List.head
