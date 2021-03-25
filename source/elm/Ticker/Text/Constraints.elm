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
    = ServeConstraints Char
    | RallyConstraints
        { initial : Char
        , incorporates : Char
        , played : List String
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
    ServeConstraints initial


rally : { initial : Char, incorporates : Char } -> Constraints
rally { initial, incorporates } =
    RallyConstraints { initial = initial, incorporates = incorporates, played = [] }



-- ACCESSORS


getInitial : Constraints -> Char
getInitial constraints =
    case constraints of
        ServeConstraints initial ->
            initial

        RallyConstraints { initial } ->
            initial


getIncorporates : Constraints -> Maybe Char
getIncorporates constraints =
    case constraints of
        ServeConstraints _ ->
            Nothing

        RallyConstraints { incorporates } ->
            Just incorporates



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
check text constraints words =
    let
        ( initial_, incorporatesM ) =
            case constraints of
                ServeConstraints initial ->
                    ( initial, Nothing )

                RallyConstraints { initial, incorporates } ->
                    ( initial, Just incorporates )
    in
    case Utils.stringHead text of
        Just head ->
            if head /= initial_ then
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
