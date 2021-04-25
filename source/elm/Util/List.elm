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


consMaybe : Maybe a -> List a -> List a
consMaybe maybe list =
    case maybe of
        Just a ->
            a :: list

        Nothing ->
            list


appendWhen : Bool -> List a -> List a -> List a
appendWhen yes listA listB =
    if yes then
        listB ++ listA

    else
        listB


padLeft : Int -> a -> List a -> List a
padLeft len filler list =
    List.repeat (len - List.length list) filler
        ++ list


padRight : Int -> a -> List a -> List a
padRight len filler list =
    list
        ++ List.repeat (len - List.length list) filler
