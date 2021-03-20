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
    = Serve
        { initial : Char
        }
    | Rally
        { initial : Char
        , incorporates : Char
        }


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
    Serve { initial = initial }


rally : { initial : Char, incorporates : Char } -> Constraints
rally cnts =
    Rally cnts



-- ACCESSORS


getInitial : Constraints -> Char
getInitial cnts =
    case cnts of
        Serve { initial } ->
            initial

        Rally { initial } ->
            initial


getIncorporates : Constraints -> Maybe Char
getIncorporates cnts =
    case cnts of
        Rally { incorporates } ->
            Just incorporates

        _ ->
            Nothing



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
check text cnts words =
    case Utils.stringHead text of
        Just head ->
            case cnts of
                Serve { initial } ->
                    if head /= initial then
                        InputInitialWrong

                    else if not (Words.exists text words) then
                        InputNotAWord

                    else
                        InputCorrect

                Rally { initial, incorporates } ->
                    if head /= initial then
                        InputInitialWrong

                    else if not (Words.exists text words) then
                        InputNotAWord

                    else if not (Utils.stringMember incorporates text) then
                        InputIncorporatesWrong

                    else
                        InputCorrect

        Nothing ->
            InputNotAWord
