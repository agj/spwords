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
            ++ "it's menacing `one`{var} against crowd favorite `two`{var}!"
    , rules =
        "remember the rules: "
            ++ "the contestants take turns to type words that *start* with the *round's letter*. "
            ++ "they also must *contain* the *last letter of the previous word*. "
            ++ "then, press enter. "
            ++ "*no repeats*, and watch the time limit! "
            ++ "first to seize *three rounds* is the victor. "
            ++ "now, let the match begin!"
    , roundStart =
        [ "let's see who comes out victorious in the next round!"
        , "now for another round full of suspense!"
        , "who will make the best of this round?"
        ]
    , turnAndLetter =
        [ "starting, it's `turn`{var} with “`letter`{var}”!"
        , "it's `turn`{var} with “`letter`{var}”!"
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
        , timeOut =
            [ "time is up!"
            , "time ran out!"
            , "no time left!"
            ]
        }
    , roundEnd =
        { point =
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
        , winning =
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
    , scoreTally =
        [ "we have our game at `one`{var} `pointsOne`{var}, `two`{var} `pointsTwo`{var}!"
        , "the panel reads `one`{var} `pointsOne`{var} versus `two`{var} `pointsTwo`{var}."
        , "`one`{var} at `pointsOne`{var}, `two`{var} at `pointsTwo`{var}!"
        ]
    , gameEnd =
        "and it's settled! `winner`{var} defeats `loser`{var} `winnerPoints`{var} to `loserPoints`{var} in a match to remember! "
            ++ "we look forward to when these two titans have another face-off. "
            ++ "see you next time!"
    }
