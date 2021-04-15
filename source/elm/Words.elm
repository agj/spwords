module Words exposing
    ( Words
    , candidate
    , exists
    , get
    , parse
    )

import Random
import Utils


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


get : Random.Seed -> Char -> Maybe Char -> Words -> ( String, Random.Seed )
get seed initial incorporatesM (Words words) =
    let
        initialPredicate word =
            case String.uncons word of
                Just ( l, rest ) ->
                    l == initial

                Nothing ->
                    False

        incorporatesPredicate word =
            case incorporatesM of
                Nothing ->
                    True

                Just incorporates ->
                    String.any ((==) incorporates) word

        filteredWords =
            words
                |> List.filter initialPredicate
                |> List.filter incorporatesPredicate
    in
    Utils.randomItem seed filteredWords
        |> Tuple.mapFirst (Maybe.withDefault "?")
