module Texts exposing (..)


title =
    "Spwords"


alphabet =
    "abcdefghijklmnopqrstuvwxyz"


names =
    { player = "player"
    , computer = "computer"
    , left = "left"
    , right = "right"
    }


comments =
    { loading = "(loading...)"
    , toStart = "(done. press *enter*.)"
    , start =
        "welcome to tonight's exciting match! "
            ++ "it's menacing {one} against favorite {two}! "
    , rules =
        "remember the rules: "
            ++ "in turns, contestants type words that start with the assigned letter, "
            ++ "and which contain the last letter of the previous word. then, press enter. "
            ++ "no repeats, and watch the time limit! "
            ++ "first to seize three rounds is the victor. "
            ++ "now, let the match begin!"
    , roundStart =
        [ "let's see who comes out victorious in the next round!"
        , "now for another round full of suspense!"
        , "who will make the best of this round?"
        ]
    , turnAndLetter =
        [ "starting, it's {turn} with “{letter}”!"
        , "it's {turn} with “{letter}”!"
        , "serving is {turn}, with “{letter}”!"
        , "here we go, turn for {turn}, with “{letter}”!"
        , "watch for that “{letter}”, {turn}!"
        , "time for “{letter}”, {turn}!"
        , "featuring “{letter}”, speaks {turn}!"
        , "we want letter “{letter}”, {turn}!"
        , "show us that “{letter}”, {turn}!"
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
        { startLetter =
            [ "did not start with “{letter}”!"
            ]
        , includeLetter =
            [ "does not contain previous word's “{letter}”!"
            ]
        , alreadyPlayed =
            [ "we've seen that word before!"
            , "that one's a repeat!"
            ]
        , doesntExist =
            [ "is that english?"
            , "no such word in my dictionary!"
            , "what does that even mean?"
            ]
        , timeOut =
            [ "time is up!"
            , "time ran out!"
            , "no more time!"
            ]
        }
    , roundEnd =
        { point =
            [ "brilliant point for {winner}!"
            , "{loser} wastes a chance!"
            , "tough luck!"
            , "what a shock!"
            , "{winner} scores!"
            , "too bad for {loser}!"
            , "close, but no dice!"
            , "it's {loser}'s miss!"
            , "{winner} takes this point!"
            ]
        , winning =
            [ "{winner} has the lead!"
            , "{winner} is ahead!"
            , "{loser} needs to step up!"
            , "{loser} is not looking good!"
            ]
        , tie =
            [ "it's tied!"
            , "{points} all!"
            , "it's a battle of noses!"
            , "both sides equal!"
            , "both with {points}!"
            ]
        }
    , scoreTally =
        [ "we have our game at {one} {pointsOne}, {two} {pointsTwo}!"
        , "the panel reads {one} {pointsOne} versus {two} {pointsTwo}."
        , "{one} at {pointsOne}, {two} at {pointsTwo}!"
        ]
    , gameEnd =
        "and it's settled! {winner} defeats {loser} {winnerPoints} to {loserPoints} in a match to remember! "
            ++ "we look forward to when these two titans have another face-off. "
            ++ "see you next time!"
    }
