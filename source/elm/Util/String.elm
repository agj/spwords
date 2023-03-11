module Util.String exposing (..)


head : String -> Maybe Char
head str =
    String.uncons str
        |> Maybe.map Tuple.first


last : String -> Maybe Char
last str =
    charAt (String.length str - 1) str


charAt : Int -> String -> Maybe Char
charAt index str =
    str
        |> String.dropLeft index
        |> head


member : Char -> String -> Bool
member char str =
    String.any ((==) char) str
