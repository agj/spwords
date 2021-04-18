module Util.List exposing (..)


unnest : List (List a) -> List a
unnest list =
    List.concatMap identity list


consWhen : Bool -> a -> List a -> List a
consWhen yes item list =
    if yes then
        item :: list

    else
        list
