module Texts exposing
    ( MistakeArguments
    , alphabet
    , alreadyPlayed
    , gameEnd
    , gameStart
    , incorporatesWrong
    , initialWrong
    , interjection
    , loadError
    , loading
    , newRound
    , notAWord
    , ready
    , roundEnd
    , roundStart
    , rules
    , tally
    , tie
    , timeUp
    , title
    , winning
    )

import Athlete exposing (..)
import Dict exposing (Dict)
import Doc
import Doc.EmuDecode
import Doc.Format as Format exposing (Format)
import Doc.Paragraph as Paragraph exposing (Paragraph)
import Doc.Text as Text exposing (Text)
import Game.GameMode exposing (GameMode(..))
import List.Extra as List
import Palette exposing (athleteA)
import Random
import Score exposing (Points, Score)
import Util.Random as Random


title =
    "Spwords"


alphabet =
    "abcdefghijklmnopqrstuvwxyz"


loading =
    comments.loading
        |> emu identity Dict.empty


loadError =
    comments.loadError
        |> emu identity Dict.empty


ready =
    comments.ready
        |> emu identity Dict.empty


gameStart : GameMode -> Paragraph
gameStart mode =
    let
        setStyles txt =
            case Text.content txt of
                "athleteA" ->
                    txt |> Text.mapFormat (Format.setAthlete (Just AthleteA))

                "athleteB" ->
                    txt |> Text.mapFormat (Format.setAthlete (Just AthleteB))

                _ ->
                    txt

        vars =
            Dict.fromList
                [ ( "athleteA", athleteName mode AthleteA )
                , ( "athleteB", athleteName mode AthleteB )
                ]
    in
    comments.gameStart
        |> emu setStyles vars


rules =
    comments.rules
        |> emu identity Dict.empty


roundStart : { turnAthlete : Athlete, mode : GameMode, initial : Char, seed : Random.Seed } -> ( Paragraph, Random.Seed )
roundStart { turnAthlete, mode, initial, seed } =
    let
        setStyles txt =
            case Text.content txt of
                "turn" ->
                    txt |> Text.mapFormat (Format.setAthlete (Just turnAthlete))

                "letter" ->
                    txt |> Text.mapFormat (Format.setBold True)

                _ ->
                    txt

        vars =
            Dict.fromList
                [ ( "turn", athleteName mode turnAthlete )
                , ( "letter", initial |> String.fromChar )
                ]
    in
    comments.roundStart
        |> randomString seed
        |> Tuple.mapFirst (emu setStyles vars)
        |> Tuple.mapFirst addLastSpace


interjection : Random.Seed -> ( Paragraph, Random.Seed )
interjection seed =
    comments.interjection
        |> emuRandomString seed identity Dict.empty
        |> Tuple.mapFirst addLastSpace


roundEnd : { winner : Athlete, mode : GameMode, seed : Random.Seed } -> ( Paragraph, Random.Seed )
roundEnd { winner, mode, seed } =
    let
        loser =
            Athlete.opposite winner

        setStyles txt =
            case Text.content txt of
                "winner" ->
                    txt |> Text.mapFormat (Format.setAthlete (Just winner))

                "loser" ->
                    txt |> Text.mapFormat (Format.setAthlete (Just loser))

                _ ->
                    txt

        vars =
            Dict.fromList
                [ ( "winner", athleteName mode winner )
                , ( "loser", athleteName mode loser )
                ]
    in
    comments.roundEnd
        |> emuRandomString seed setStyles vars


tally : { mode : GameMode, pointsA : Points, pointsB : Points, seed : Random.Seed } -> ( Paragraph, Random.Seed )
tally { mode, pointsA, pointsB, seed } =
    let
        setStyles txt =
            case Text.content txt of
                "athleteA" ->
                    txt |> Text.mapFormat (Format.setAthlete (Just AthleteA))

                "athleteB" ->
                    txt |> Text.mapFormat (Format.setAthlete (Just AthleteB))

                _ ->
                    txt |> Text.mapFormat (Format.setBold True)

        vars =
            Dict.fromList
                [ ( "athleteA", athleteName mode AthleteA )
                , ( "athleteB", athleteName mode AthleteB )
                , ( "pointsA", pointsA |> Score.intFromPoints |> String.fromInt )
                , ( "pointsB", pointsB |> Score.intFromPoints |> String.fromInt )
                ]
    in
    comments.tally
        |> emuRandomString seed setStyles vars


winning : { winner : Athlete, mode : GameMode, seed : Random.Seed } -> ( Paragraph, Random.Seed )
winning { winner, mode, seed } =
    let
        loser =
            Athlete.opposite winner

        setStyles txt =
            case Text.content txt of
                "winner" ->
                    txt |> Text.mapFormat (Format.setAthlete (Just winner))

                "loser" ->
                    txt |> Text.mapFormat (Format.setAthlete (Just loser))

                _ ->
                    txt

        vars =
            Dict.fromList
                [ ( "winner", athleteName mode winner )
                , ( "loser", athleteName mode loser )
                ]
    in
    comments.assessment.winning
        |> emuRandomString seed setStyles vars


tie : { points : Points, seed : Random.Seed } -> ( Paragraph, Random.Seed )
tie { points, seed } =
    let
        setStyles =
            Text.mapFormat (Format.setBold True)

        vars =
            Dict.fromList
                [ ( "points", points |> Score.intFromPoints |> String.fromInt )
                ]
    in
    comments.assessment.tie
        |> emuRandomString seed setStyles vars


newRound : Random.Seed -> ( Paragraph, Random.Seed )
newRound seed =
    comments.newRound
        |> emuRandomString seed identity Dict.empty


gameEnd : { winner : Athlete, mode : GameMode, loserPoints : Points } -> Paragraph
gameEnd { winner, mode, loserPoints } =
    let
        loser =
            Athlete.opposite winner

        setStyles txt =
            case Text.content txt of
                "winner" ->
                    txt |> Text.mapFormat (Format.setAthlete (Just winner))

                "loser" ->
                    txt |> Text.mapFormat (Format.setAthlete (Just loser))

                _ ->
                    txt |> Text.mapFormat (Format.setBold True)

        vars =
            Dict.fromList <|
                [ ( "winner", athleteName mode winner )
                , ( "loser", athleteName mode loser )
                , ( "winnerPoints", Score.winPoints |> String.fromInt )
                , ( "loserPoints", loserPoints |> Score.intFromPoints |> String.fromInt )
                ]
    in
    comments.gameEnd
        |> emu setStyles vars



-- MISTAKES


initialWrong : MistakeArguments -> ( Paragraph, Random.Seed )
initialWrong arguments =
    mistake comments.mistake.initial arguments


incorporatesWrong : MistakeArguments -> ( Paragraph, Random.Seed )
incorporatesWrong arguments =
    mistake comments.mistake.incorporates arguments


alreadyPlayed : MistakeArguments -> ( Paragraph, Random.Seed )
alreadyPlayed arguments =
    mistake comments.mistake.alreadyPlayed arguments


notAWord : MistakeArguments -> ( Paragraph, Random.Seed )
notAWord arguments =
    mistake comments.mistake.notAWord arguments


timeUp : MistakeArguments -> ( Paragraph, Random.Seed )
timeUp arguments =
    mistake comments.mistake.timeUp arguments



-- INTERNAL


comments =
    { loading = "/loading.../"
    , loadError = "/error loading! please refresh./"
    , ready = "/ready. press here./"
    , gameStart =
        "welcome to tonight's exciting match! "
            ++ "it's menacing `athleteB`{var} against crowd favorite `athleteA`{var}!"
    , rules =
        "remember the rules: "
            ++ "the contestants take turns to type words that *start* with the *round's letter*. "
            ++ "they also must *contain* the *last letter of the previous word*. "
            ++ "then, press enter. "
            ++ "*no repeats*, and watch the time limit! "
            ++ "first to seize *three rounds* is the victor. "
            ++ "now, let the match begin!"
    , roundStart =
        [ "starting, it's `turn`{var} with “`letter`{var}”!"
        , "serving is `turn`{var}, with “`letter`{var}”!"
        , "here we go, turn for `turn`{var}, with “`letter`{var}”!"
        , "watch for that “`letter`{var}”, `turn`{var}!"
        , "time for “`letter`{var}”, `turn`{var}!"
        , "featuring “`letter`{var}”, speaks `turn`{var}!"
        , "we want letter “`letter`{var}”, `turn`{var}!"
        , "show us that “`letter`{var}”, `turn`{var}!"
        ]
    , interjection =
        [ "ooh!"
        , "nice!"
        , "good!"
        , "safe!"
        , "wow!"
        , "works!"
        , "fair!"
        ]
    , mistake =
        { initial =
            [ "did not start with “`initial`{var}”!"
            , "must start with “`initial`{var}”!"
            , "forgot to start with “`initial`{var}”!"
            , "but it should start with “`initial`{var}”!"
            ]
        , incorporates =
            [ "does not contain previous word's “`incorporates`{var}”!"
            , "it doesn't include the mandatory “`incorporates`{var}”!"
            , "forgot to include “`incorporates`{var}” from the previous word!"
            ]
        , alreadyPlayed =
            [ "we've seen that word before!"
            , "that one's a repeat!"
            , "that's been played already!"
            , "word's not fresh!"
            ]
        , notAWord =
            [ "is that english?"
            , "no such word in my dictionary!"
            , "what does that even mean?"
            ]
        , timeUp =
            [ "time is up!"
            , "time ran out!"
            , "no time left!"
            ]
        }
    , roundEnd =
        [ "brilliant point for `winner`{var}!"
        , "`loser`{var} wastes a chance!"
        , "tough luck!"
        , "what a shock!"
        , "`winner`{var} scores!"
        , "too bad for `loser`{var}!"
        , "close, but no dice!"
        , "it's `loser`{var}'s miss!"
        , "`winner`{var} takes this point!"
        ]
    , tally =
        [ "we have our game at `athleteA`{var} `pointsA`{var}, `athleteB`{var} `pointsB`{var}!"
        , "the panel reads `athleteA`{var} `pointsA`{var} versus `athleteB`{var} `pointsB`{var}."
        , "`athleteA`{var} at `pointsA`{var}, `athleteB`{var} at `pointsB`{var}!"
        ]
    , assessment =
        { winning =
            [ "`winner`{var} has the lead!"
            , "`winner`{var} is ahead!"
            , "`loser`{var} needs to step up!"
            , "`loser`{var} is not looking good!"
            ]
        , tie =
            [ "it's tied!"
            , "`points`{var} all!"
            , "it's a battle of noses!"
            , "both sides equal!"
            , "both with `points`{var}!"
            ]
        }
    , newRound =
        [ "let's see who comes out victorious in the next round!"
        , "now for another round full of suspense!"
        , "who will make the best of this round?"
        ]
    , gameEnd =
        "and it's settled! `winner`{var} defeats `loser`{var} `winnerPoints`{var} to `loserPoints`{var} in a match to remember! "
            ++ "we look forward to when these two titans have another face-off. "
            ++ "see you next time!"
    }


emu : (Text -> Text) -> Dict String String -> String -> Paragraph
emu formatter vars str =
    Doc.EmuDecode.fromEmu (" " ++ str)
        |> Doc.content
        |> List.head
        |> Maybe.withDefault Paragraph.empty
        |> replaceVars formatter vars


replaceVars : (Text -> Text) -> Dict String String -> Paragraph -> Paragraph
replaceVars formatter vars par =
    let
        replaceVar txt =
            let
                content =
                    Text.content txt
            in
            if txt |> Text.format |> Format.isVar then
                txt
                    |> Text.mapFormat (Format.setVar False)
                    |> formatter
                    |> Text.setContent
                        (Dict.get content vars |> Maybe.withDefault content)

            else
                txt
    in
    par
        |> Paragraph.mapContent (List.map replaceVar)


emuRandomString : Random.Seed -> (Text -> Text) -> Dict String String -> List String -> ( Paragraph, Random.Seed )
emuRandomString seed formatter vars strings =
    randomString seed strings
        |> Tuple.mapFirst (emu formatter vars)


randomString : Random.Seed -> List String -> ( String, Random.Seed )
randomString seed strings =
    Random.item seed strings
        |> Tuple.mapFirst (Maybe.withDefault "")


type alias MistakeArguments =
    { initial : Char
    , incorporates : Maybe Char
    , seed : Random.Seed
    }


mistake : List String -> MistakeArguments -> ( Paragraph, Random.Seed )
mistake texts { initial, incorporates, seed } =
    let
        vars =
            case incorporates of
                Just char ->
                    Dict.fromList
                        [ ( "initial", initial |> String.fromChar )
                        , ( "incorporates", char |> String.fromChar )
                        ]

                Nothing ->
                    Dict.fromList
                        [ ( "initial", initial |> String.fromChar )
                        ]

        setStyles =
            Text.mapFormat (Format.setBold True)
    in
    texts
        |> emuRandomString seed setStyles vars


addLastSpace : Paragraph -> Paragraph
addLastSpace par =
    let
        add texts =
            case ( List.init texts, List.last texts ) of
                ( Just init, Just last ) ->
                    init ++ [ last |> Text.mapContent (\str -> str ++ " ") ]

                _ ->
                    texts
    in
    Paragraph.mapContent add par


athleteName : GameMode -> Athlete -> String
athleteName mode athlete =
    case mode of
        HotseatMode ->
            case athlete of
                AthleteA ->
                    "left"

                AthleteB ->
                    "right"

        SingleMode ->
            case athlete of
                AthleteA ->
                    "player"

                AthleteB ->
                    "computer"
