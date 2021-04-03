module Constraints exposing
    ( CandidateCheck(..)
    , Constraints
    , InputCheck(..)
    , check
    , checkCandidate
    , getIncorporates
    , getInitial
    , getPlayed
    , pushPlayed
    , rally
    , serve
    , setIncorporates
    , setInitial
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
    | InputAlreadyPlayed
    | InputNotAWord



-- CONSTRUCTORS


serve : Char -> Constraints
serve initial =
    ServeConstraints initial


rally : { initial : Char, incorporates : Char, played : List String } -> Constraints
rally { initial, incorporates, played } =
    RallyConstraints { initial = initial, incorporates = incorporates, played = played }



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


getPlayed : Constraints -> List String
getPlayed constraints =
    case constraints of
        ServeConstraints _ ->
            []

        RallyConstraints { played } ->
            played



-- MODIFICATION


setInitial : Char -> Constraints -> Constraints
setInitial initial constraints =
    case constraints of
        ServeConstraints _ ->
            ServeConstraints initial

        RallyConstraints ral ->
            RallyConstraints { ral | initial = initial }


setIncorporates : Char -> Constraints -> Constraints
setIncorporates incorporates constraints =
    case constraints of
        ServeConstraints ini ->
            RallyConstraints
                { initial = ini
                , incorporates = incorporates
                , played = []
                }

        RallyConstraints ral ->
            RallyConstraints { ral | incorporates = incorporates }


pushPlayed : String -> Constraints -> Constraints
pushPlayed word constraints =
    case Utils.stringLast word of
        Just char ->
            case constraints of
                ServeConstraints ini ->
                    RallyConstraints
                        { initial = ini
                        , incorporates = char
                        , played = [ word ]
                        }

                RallyConstraints ral ->
                    RallyConstraints
                        { ral
                            | incorporates = char
                            , played = word :: ral.played
                        }

        Nothing ->
            constraints



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
        ( initial_, incorporatesM, played_ ) =
            case constraints of
                ServeConstraints initial ->
                    ( initial, Nothing, [] )

                RallyConstraints { initial, incorporates, played } ->
                    ( initial, Just incorporates, played )
    in
    case Utils.stringHead text of
        Just head ->
            if head /= initial_ then
                InputInitialWrong

            else if List.member text played_ then
                InputAlreadyPlayed

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
