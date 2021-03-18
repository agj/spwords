module Words exposing
    ( Words
    , candidate
    , exists
    , parse
    )

import Debug


type Words
    = Words (List String)



-- CONSTRUCTORS


parse : String -> Words
parse data =
    data
        |> String.lines
        |> List.filter ((/=) "")
        |> List.map String.toLower
        |> Words



--


exists : String -> Words -> Bool
exists word (Words words) =
    words |> List.member word


candidate : String -> Words -> Bool
candidate text (Words words) =
    let
        len =
            String.length text
    in
    words |> List.any (\w -> String.left len w == text)
