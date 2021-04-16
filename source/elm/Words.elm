module Words exposing
    ( Words
    , all
    , candidate
    , exists
    , getByInitial
    , getRandom
    , parse
    )

import Dict exposing (Dict)
import Dict.Extra as Dict
import Random
import Utils exposing (stringHead)


type Words
    = Words (Dict Char (List String))



-- CONSTRUCTORS


parse : String -> Words
parse data =
    let
        wordToDict : String -> Dict Char (List String) -> Dict Char (List String)
        wordToDict word dict =
            let
                initial =
                    Utils.stringHead word
                        |> Maybe.withDefault '?'
            in
            Dict.update initial (wordToList word) dict

        wordToList : String -> Maybe (List String) -> Maybe (List String)
        wordToList word listM =
            case listM of
                Just list ->
                    Just (word :: list)

                Nothing ->
                    Just [ word ]
    in
    data
        |> String.lines
        |> List.filter ((/=) "")
        |> List.map String.toLower
        |> List.foldl wordToDict Dict.empty
        |> Words



--


all : Words -> List String
all (Words words) =
    Dict.foldl
        (\_ list acc -> acc ++ list)
        []
        words


exists : String -> Words -> Bool
exists word (Words words) =
    words |> Dict.any (\_ list -> List.member word list)


candidate : String -> Words -> Bool
candidate text words =
    let
        initial =
            stringHead text |> Maybe.withDefault '?'

        len =
            String.length text

        matches w =
            String.left len w == text
    in
    words
        |> getByInitial initial
        |> List.any matches


getRandom : Random.Seed -> Char -> Maybe Char -> Words -> ( String, Random.Seed )
getRandom seed initial incorporatesM words =
    let
        incorporatesPredicate =
            case incorporatesM of
                Nothing ->
                    always True

                Just incorporates ->
                    \word ->
                        String.any ((==) incorporates) word

        filteredWords =
            words
                |> getByInitial initial
                |> List.filter incorporatesPredicate
    in
    Utils.randomItem seed filteredWords
        |> Tuple.mapFirst (Maybe.withDefault "?")


getByInitial : Char -> Words -> List String
getByInitial initial (Words words) =
    words
        |> Dict.get initial
        |> Maybe.withDefault []
