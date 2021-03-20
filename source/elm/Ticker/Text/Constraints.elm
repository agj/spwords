module Ticker.Text.Constraints exposing (..)

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


type InputCheck
    = InputCorrect
    | InputInitialWrong
    | InputIncorporatesWrong
    | InputNotAWord


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
