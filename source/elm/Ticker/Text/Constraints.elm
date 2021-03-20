module Ticker.Text.Constraints exposing
    ( CandidateCheck(..)
    , Constraints
    , InputCheck(..)
    , check
    , checkCandidate
    , getIncorporates
    , getInitial
    , rally
    , serve
    )

import Utils
import Words exposing (Words)


type Constraints
    = Constraints Char (Maybe Char)


type CandidateCheck
    = CandidateCorrect
    | CandidateInitialWrong
    | CandidateNotAWord


type InputCheck
    = InputCorrect
    | InputInitialWrong
    | InputIncorporatesWrong
    | InputNotAWord



-- CONSTRUCTORS


serve : Char -> Constraints
serve initial =
    Constraints initial Nothing


rally : { initial : Char, incorporates : Char } -> Constraints
rally { initial, incorporates } =
    Constraints initial (Just incorporates)



-- ACCESSORS


getInitial : Constraints -> Char
getInitial (Constraints initial _) =
    initial


getIncorporates : Constraints -> Maybe Char
getIncorporates (Constraints _ incorporatesM) =
    incorporatesM



-- CHECKS


checkCandidate : String -> Constraints -> Words -> CandidateCheck
checkCandidate text cnts words =
    case Utils.stringHead text of
        Just head ->
            if head /= getInitial cnts then
                CandidateInitialWrong

            else if not (Words.candidate text words) then
                CandidateNotAWord

            else
                CandidateCorrect

        Nothing ->
            CandidateCorrect


check : String -> Constraints -> Words -> InputCheck
check text (Constraints initial incorporatesM) words =
    case Utils.stringHead text of
        Just head ->
            if head /= initial then
                InputInitialWrong

            else if not (Words.exists text words) then
                InputNotAWord

            else
                case incorporatesM of
                    Just incorporates ->
                        if not (Utils.stringMember incorporates text) then
                            InputIncorporatesWrong

                        else
                            InputCorrect

                    Nothing ->
                        InputCorrect

        Nothing ->
            InputNotAWord
