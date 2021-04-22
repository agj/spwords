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

import Util.String as String
import Words exposing (Words)


type Constraints
    = ServeConstraints
        { initial : Char
        , played : List String
        }
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


serve : { initial : Char, played : List String } -> Constraints
serve { initial, played } =
    ServeConstraints { initial = initial, played = played }


rally : { initial : Char, incorporates : Char, played : List String } -> Constraints
rally { initial, incorporates, played } =
    RallyConstraints { initial = initial, incorporates = incorporates, played = played }



-- ACCESSORS


getInitial : Constraints -> Char
getInitial constraints =
    case constraints of
        ServeConstraints { initial } ->
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
        ServeConstraints { played } ->
            played

        RallyConstraints { played } ->
            played



-- MODIFICATION


setInitial : Char -> Constraints -> Constraints
setInitial initial constraints =
    case constraints of
        ServeConstraints ser ->
            ServeConstraints { ser | initial = initial }

        RallyConstraints ral ->
            RallyConstraints { ral | initial = initial }


setIncorporates : Char -> Constraints -> Constraints
setIncorporates incorporates constraints =
    case constraints of
        ServeConstraints { initial, played } ->
            RallyConstraints
                { initial = initial
                , played = played
                , incorporates = incorporates
                }

        RallyConstraints ral ->
            RallyConstraints { ral | incorporates = incorporates }


pushPlayed : String -> Constraints -> Constraints
pushPlayed word constraints =
    case String.last word of
        Just char ->
            case constraints of
                ServeConstraints { initial, played } ->
                    RallyConstraints
                        { initial = initial
                        , played = word :: played
                        , incorporates = char
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
    case String.head text of
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
                ServeConstraints { initial, played } ->
                    ( initial, Nothing, played )

                RallyConstraints { initial, incorporates, played } ->
                    ( initial, Just incorporates, played )
    in
    case String.head text of
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
                        if not (String.member incorporates text) then
                            InputIncorporatesWrong

                        else
                            InputCorrect

                    Nothing ->
                        InputCorrect

        Nothing ->
            InputNotAWord
