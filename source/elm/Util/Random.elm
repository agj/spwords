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


with2 : Generator a -> (a -> Generator b) -> ( a -> b -> c, Seed ) -> ( c, Seed )
with2 gen fn ( receiver, seed ) =
    let
        ( first, seed1 ) =
            Random.step gen seed

        ( second, seed2 ) =
            Random.step (fn first) seed1
    in
    ( receiver first second
    , seed2
    )


get : Seed -> Generator a -> ( a, Seed )
get seed gen =
    Random.step gen seed
